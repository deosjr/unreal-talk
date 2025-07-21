package main

/*
#cgo CFLAGS: -I../apriltag
#cgo LDFLAGS: -L../apriltag/build -lapriltag

#include "detect_tags.c"
#include "../apriltag/apriltag.h"
#include "../apriltag/tagStandard41h12.h"
#include <stdlib.h>

Detection* detect_tags(uint8_t* gray, int width, int height, int* num);
*/
import "C"
import (
	"fmt"
	"image"
	"image/color"
	"unsafe"

	"gocv.io/x/gocv"
)

func main() {
	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()

	window := gocv.NewWindow("Webcam")
	defer window.Close()
	projector := gocv.NewWindow("Projection")
	defer projector.Close()
	fullscreen := true

	img := gocv.NewMat()
	defer img.Close()
	x, y := 1920, 1080
	projection := gocv.NewMatWithSize(y, x, gocv.MatTypeCV8UC3)
	defer projection.Close()

	fs := gocv.NewFileStorage()
	defer fs.Close()

	data := []float64{
1.4278061986826285, 0.12211516572981373, -675.4491964319097,
-0.004947863817419483, 1.5273173353701337, -390.3351994799858,
-1.0755690467340253e-05, 0.00016574757933757663, 1,
    	}
	
    	homography := gocv.NewMatWithSize(3, 3, gocv.MatTypeCV64F)

    	for i := 0; i < 3; i++ {
    	    for j := 0; j < 3; j++ {
    	    	homography.SetDoubleAt(i, j, data[3*i+j])
    	    }
    	}

	for {
		if ok := webcam.Read(&img); !ok || img.Empty() {
			continue
		}
		projection.SetTo(gocv.NewScalar(0, 0, 0, 0))

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

		slice := (*[1 << 10]C.Detection)(unsafe.Pointer(dets))[:num:num]
		for _, d := range slice {
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
			gocv.Circle(&img, points[0], 5, color.RGBA{255, 0, 0, 0}, 2)
			gocv.Circle(&img, points[1], 5, color.RGBA{255, 0, 0, 0}, 2)
			gocv.Circle(&img, points[2], 5, color.RGBA{255, 0, 0, 0}, 2)
			gocv.Circle(&img, points[3], 5, color.RGBA{255, 0, 0, 0}, 2)

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
			polygons := gocv.NewPointsVector()
			polygons.Append(dstPoints)
			gocv.FillPoly(&projection, polygons, color.RGBA{R: 255, A: 255})
		}

		window.IMShow(img)
		projector.IMShow(projection)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
		if projector.WaitKey(1) == 102 { // f
			fullscreen = !fullscreen
			if fullscreen {
			    projector.SetWindowProperty(gocv.WindowPropertyFullscreen, gocv.WindowFullscreen)
			} else {
			    projector.SetWindowProperty(gocv.WindowPropertyFullscreen, gocv.WindowNormal)
			}
		}
	}
}

