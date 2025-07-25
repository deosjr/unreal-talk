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
	"time"
	"unsafe"

	"gocv.io/x/gocv"
)

func scm_point(p image.Point) C.SCM {
	return C.scm_cons(C.scm_from_int(C.int(p.X)), C.scm_from_int(C.int(p.Y)))
}

func scm_pageGeometry(pg pageGeometry) C.SCM {
	id := C.scm_from_int(C.int(pg.id))
	ulhc :=  scm_point(pg.ulhc)
	urhc :=  scm_point(pg.urhc)
	llhc :=  scm_point(pg.llhc)
	lrhc :=  scm_point(pg.lrhc)
	points := C.scm_list_4(ulhc, urhc, llhc, lrhc)
	rotation := C.scm_from_double(C.double(pg.rotation))
	return C.scm_list_3(id, points, rotation)
}

// todo: sends key pressed info as well, perhaps rename?
func scm_sendPageGeometries(geos []pageGeometry, key int) {
	fname := C.CString("pages-found")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	list := C.SCM_EOL
	for i:=len(geos)-1; i >=0; i-- {
		pg := scm_pageGeometry(geos[i])
		list = C.scm_cons(pg, list)
	}
	C.scm_call_2(f, list, C.scm_from_int(C.int(key)))
}

func scm_sendImagePointer(img gocv.Mat) {
	ptr := unsafe.Pointer(img.Ptr())
	fname := C.CString("init-image")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_1(f, C.scm_from_pointer(ptr, nil))
}

type pageGeometry struct {
	id int
	ulhc, urhc, llhc, lrhc image.Point
	rotation float64
}

func main() {

	calib := flag.Bool("calibrate", false, "calibrate camera instead of running")
	flag.Parse()
	if *calib {
		fmt.Println("calibrating...")
		calibrate()
		return
	}

	C.scm_init_guile()
	C.scm_c_primitive_load(C.CString("test.scm"))

	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()

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

	// send the projection pointer to Guile, once.
	// it should not free it, that's still Go's job
	// todo: send projector dimensions as well?
	scm_sendImagePointer(projection)

	projector.IMShow(projection)
	fmt.Println("drag projector screen to projector, then press 'f' to fullscreen")
	for {
		if projector.WaitKey(1) == 102 { // f
		    	projector.SetWindowProperty(gocv.WindowPropertyFullscreen, gocv.WindowFullscreen)
			break
		}
	}

	data := []float64{
1.2580602714027673, 0.10159008314992518, -499.2400878211504,
-0.04726029080388189, 1.3636369977843847, -32.42505965294924,
-4.052051859479486e-05, 0.00011646813994832703, 1,
    	}
	
    	homography := gocv.NewMatWithSize(3, 3, gocv.MatTypeCV64F)

    	for i := 0; i < 3; i++ {
    	    for j := 0; j < 3; j++ {
    	    	homography.SetDoubleAt(i, j, data[3*i+j])
    	    }
    	}

	for {
		start := time.Now()
		// todo: use some kind of buffer instead of sampling?
		// probably better in a separate background process
		keyDown := gocv.WaitKey(1)
		if ok := webcam.Read(&img); !ok || img.Empty() {
			continue
		}

		// Convert to grayscale
		gray := gocv.NewMat()
		gocv.CvtColor(img, &gray, gocv.ColorBGRToGray)
		defer gray.Close()
		// TODO: perhaps transform like calibrate does into black/white threshold?
		// obviously without inversion of black/white!

		data := gray.ToBytes()

		var num C.int
		dets := C.detect_tags((*C.uint8_t)(unsafe.Pointer(&data[0])), C.int(gray.Cols()), C.int(gray.Rows()), &num)
		defer C.free(unsafe.Pointer(dets))
		time_detect := time.Now().Sub(start)
		start = time.Now()

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
		time_geos := time.Now().Sub(start)
		start = time.Now()
		if key := gocv.WaitKey(1); key != -1 {
			keyDown = key
		}
		scm_sendPageGeometries(pageGeos, keyDown)
		time_scm := time.Now().Sub(start)
		pageids := []int{}
		for _, pg := range pageGeos {
			pageids = append(pageids, pg.id)
		}
		fmt.Printf("%v\t%v\t%v - %v\n", time_detect.Round(time.Millisecond), time_geos.Round(time.Microsecond), time_scm.Round(time.Millisecond), pageids)

		// doesn't show up because fullscreen projection still somehow clips
		gocv.Rectangle(&projection, image.Rect(0, 0, x, y), color.RGBA{255,255,255,255}, 2)
		window.IMShow(img)
		projector.IMShow(projection)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
	}
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
