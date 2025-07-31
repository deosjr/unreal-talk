package main

/*
#cgo CFLAGS: -I../apriltag
#cgo LDFLAGS: -L../apriltag/build -lapriltag
#cgo pkg-config: opencv4

#include "detect_tags.h"
#include "../apriltag/apriltag.h"
#include "../apriltag/tagStandard41h12.h"
#include <stdlib.h>
#include <libguile.h>
*/
import "C"
import (
	"flag"
	"fmt"
	"image"
	"image/color"
	"math"
	"sort"
	"time"
	"unsafe"

	"gocv.io/x/gocv"
)

func main() {

	calib := flag.Bool("calibrate", false, "calibrate camera instead of running")
	flag.Parse()
	if *calib {
		fmt.Println("calibrating...")
		calibrate()
		return
	}

	C.scm_init_guile()
	C.scm_c_primitive_load(C.CString("main.scm"))

	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()
	// todo: doesn't seem to work, at least on my machine with my webcam
	/*
	webcam.Set(gocv.VideoCaptureAutoWB, 0.0)
	webcam.Set(gocv.VideoCaptureAutoFocus, 0.0)
	webcam.Set(gocv.VideoCaptureAutoExposure, 0.0)
	webcam.Set(gocv.VideoCaptureWBTemperature, 3000.0) 	// 2800 - 6500 ?
	webcam.Set(gocv.VideoCaptureFocus, 100.0)		// 0-255 ?
	webcam.Set(gocv.VideoCaptureExposure, -3.0)		// ?
	*/

	window := gocv.NewWindow("Webcam")
	defer window.Close()
	projector := gocv.NewWindow("Projection")
	defer projector.Close()

	img := gocv.NewMat()
	defer img.Close()
	// calibration pattern fullscreen fills projector dimensions
	// projection fullscreen should match those dimensions!
	// todo: read dimensions from the calibration output?
	x, y := 1280, 720
	projection := gocv.NewMatWithSize(y, x, gocv.MatTypeCV8UC3)
	defer projection.Close()

	projector.IMShow(projection)
	fmt.Println("drag projector screen to projector, then press 'f' to fullscreen")
	for {
		if projector.WaitKey(1) == 102 { // f
		    	projector.SetWindowProperty(gocv.WindowPropertyFullscreen, gocv.WindowFullscreen)
			break
		}
	}

	data := []float64{
1.3883089177554742, 0.08500871618335931, -744.1644416307363,
-0.022095358148125955, 1.408334851255064, -215.54421787624605,
1.1986272425640062e-05, 8.685501637361924e-05, 1,
    	}
	
    	homography := gocv.NewMatWithSize(3, 3, gocv.MatTypeCV64F)

    	for i := 0; i < 3; i++ {
    	    for j := 0; j < 3; j++ {
    	    	homography.SetDoubleAt(i, j, data[3*i+j])
    	    }
    	}

	// send the projection pointer to Guile, once.
	// it should not free it, that's still Go's job
	scm_sendImageInfo(img, projection, homography, x, y)

	detector := C.new_detector()
	defer C.free_detector(detector)

	var time_detect time.Duration
	var time_scm time.Duration
	
Loop:
	for {
		start := time.Now()
		keyDown := -1
		ch := make(chan []pageGeometry, 1)
		go aprilTagDetection(detector, webcam, img, homography, x, y, ch)
		for {
			select {
			case pgs := <-ch:
				time_detect = time.Now().Sub(start)
				start = time.Now()
				scm_sendTimeDetect(time_detect.Milliseconds())
				scm_sendTimeSCM(time_scm.Milliseconds())
				scm_sendKeyDown(keyDown)
				scm_sendPageGeometries(pgs)
				time_scm = time.Now().Sub(start)
				tagids := []int{}
				for _, tag := range pgs {
					tagids = append(tagids, tag.id)
				}
				sort.Ints(tagids)
				fmt.Printf("%v\t%v\t%v\n", time_detect.Round(time.Millisecond), time_scm.Round(time.Millisecond), tagids)

				// doesn't show up because fullscreen projection still somehow clips
				gocv.Rectangle(&projection, image.Rect(0, 0, x, y), color.RGBA{255,255,255,255}, 2)
				window.IMShow(img)
				projector.IMShow(projection)
				continue Loop
			default:
				if key := gocv.WaitKey(1); key != -1 {
					keyDown = key
				}
			}
		}
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
	}
}

type pageGeometry struct {
	id int
	ulhc, urhc, llhc, lrhc image.Point
	rotation float64
}

// runs in a goroutine because keyboard event listener has to run in main thread
func aprilTagDetection(detector *C.Detector, webcam *gocv.VideoCapture, img, homography gocv.Mat, x, y int, resch chan []pageGeometry) {
	// todo: use some kind of buffer instead of sampling?
	// probably better in a separate background process
	if ok := webcam.Read(&img); !ok || img.Empty() {
		resch <- nil
		return
	}

	// Convert to grayscale
	gray := gocv.NewMat()
	gocv.CvtColor(img, &gray, gocv.ColorBGRToGray)
	defer gray.Close()
	// todo: sometimes helps, sometimes hurts detection ?
	//gocv.GaussianBlur(gray, &gray, image.Pt(5, 5), 0, 0, gocv.BorderDefault)
	//gocv.Threshold(gray, &gray, 160, 255, gocv.ThresholdBinary)

	data := gray.ToBytes()

	var num C.int
	dets := C.detect_tags(detector, (*C.uint8_t)(unsafe.Pointer(&data[0])), C.int(gray.Cols()), C.int(gray.Rows()), &num)
	defer C.free(unsafe.Pointer(dets))

	pageGeos := []pageGeometry{}

	slice := (*[1 << 10]C.Detection)(unsafe.Pointer(dets))[:num:num]
	ch := make(chan detectionResult, len(slice))
	for _, d := range slice {
		go func(d C.Detection) {
			ch <- parseDetection(img, x, y, homography, d)
		}(d)
	}
	for i:=0; i<len(slice); i++ {
		result := <-ch
		if !result.ok {
			continue
		}
		pageGeos = append(pageGeos, result.pageGeo)
	}
	close(ch)
	resch <- pageGeos
}

type detectionResult struct {
	pageGeo pageGeometry
	ok	bool
}

func parseDetection(img gocv.Mat, x, y int, homography gocv.Mat, d C.Detection) detectionResult {
	// todo: camera calibration (estimate?) + apriltag estimate_tag_pose
	center := image.Pt(int(d.cx), int(d.cy))
	// The corners of the tag in image pixel coordinates. These always
	// wrap counter-clock wise around the tag.
	points := []image.Point{
		image.Pt(int(d.corners[0][0]), int(d.corners[0][1])),
		image.Pt(int(d.corners[1][0]), int(d.corners[1][1])),
		image.Pt(int(d.corners[2][0]), int(d.corners[2][1])),
		image.Pt(int(d.corners[3][0]), int(d.corners[3][1])),
	}
	// todo: this shows when capturing webcam appearance, but is debug only info!
	// solution: copy img to debug mat and draw that on debug window?
	gocv.Circle(&img, center, 5, color.RGBA{0, 255, 0, 0}, 2)
	gocv.PutText(&img, fmt.Sprintf("ID %d", d.id), center, gocv.FontHersheyPlain, 1.2, color.RGBA{255, 0, 0, 0}, 2)
	// point order seems to be: LLHC, LRHC, URHC, ULHC, stable through rotation (!)
	// todo: is that true for all tags?
	//gocv.Circle(&img, points[0], 5, color.RGBA{255, 0, 0, 0}, 2)
	//gocv.Circle(&img, points[1], 5, color.RGBA{255, 0, 0, 0}, 2)
	//gocv.Circle(&img, points[2], 5, color.RGBA{255, 0, 0, 0}, 2)
	//gocv.Circle(&img, points[3], 5, color.RGBA{255, 0, 0, 0}, 2)

	points2f := []gocv.Point2f{
		{X:float32(d.corners[0][0]), Y:float32(d.corners[0][1])},
		{X:float32(d.corners[1][0]), Y:float32(d.corners[1][1])},
		{X:float32(d.corners[2][0]), Y:float32(d.corners[2][1])},
		{X:float32(d.corners[3][0]), Y:float32(d.corners[3][1])},
	}
	pv2f := gocv.NewPoint2fVectorFromPoints(points2f)
	webcamPoints := gocv.NewMatFromPoint2fVector(pv2f, true)
	projectionPoints := gocv.NewMat()
	gocv.PerspectiveTransform(webcamPoints, &projectionPoints, homography)	
	dstPoints := gocv.NewPointVectorFromMat(projectionPoints)

	// only consider tags within the projection boundary
	bound := gocv.BoundingRect(dstPoints)
	if !bound.In(image.Rect(0, 0, x, y)) {
		return detectionResult{}
	}
	projected := dstPoints.ToPoints()

	dx := float64(points[2].X - points[3].X)
	dy := float64(points[2].Y - points[3].Y)
	angle := math.Atan2(dy, dx)
	if angle < 0 {
		angle += 2 * math.Pi
	}
	degrees := angle * 180 / math.Pi

	return detectionResult{
		pageGeo: pageGeometry{
			id: int(d.id),
			ulhc: projected[3],
			urhc: projected[2],
			llhc: projected[0],
			lrhc: projected[1],
			rotation: degrees,
		},
		ok: true,
	}
}
