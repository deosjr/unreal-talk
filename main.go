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
	"fmt"
	"image"
	"image/color"
	"math"
	"unsafe"

	"gocv.io/x/gocv"
)

func scm_point(p image.Point) C.SCM {
	return C.scm_cons(C.scm_from_int(C.int(p.X)), C.scm_from_int(C.int(p.Y)))
}

func scm_sendPage(id int, ulhc, urhc, llhc, lrhc image.Point, rotation float64) {
	fname := C.CString("page-found")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_6(f, C.scm_from_int(C.int(id)), scm_point(ulhc), scm_point(urhc), scm_point(llhc), scm_point(lrhc), C.scm_from_double(C.double(rotation)))
}

func main() {
	C.scm_init_guile()
	C.scm_c_primitive_load(C.CString("test.scm"))

	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()

	window := gocv.NewWindow("Webcam")
	defer window.Close()
	projector := gocv.NewWindow("Projection")
	defer projector.Close()
	fullscreen := true

	img := gocv.NewMat()
	defer img.Close()
	// calibration pattern fullscreen fills projector dimensions
	// projection fullscreen should match those dimensions!
	x, y := 1280, 720
	projection := gocv.NewMatWithSize(y, x, gocv.MatTypeCV8UC3)
	defer projection.Close()

	fs := gocv.NewFileStorage()
	defer fs.Close()

	data := []float64{
1.422374988730949, 0.13439035308003228, -589.9962751807983,
-0.00744007547773715, 1.5367645308852576, -288.2280970029973,
-1.127043317257658e-05, 0.0001650885739257573, 1,
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
			//gocv.Circle(&img, points[0], 5, color.RGBA{255, 0, 0, 0}, 2)
			//gocv.Circle(&img, points[1], 5, color.RGBA{255, 0, 0, 0}, 2)
			//gocv.Circle(&img, points[2], 5, color.RGBA{255, 0, 0, 0}, 2)
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

			dx := float64(points[2].X - points[3].X)
			dy := float64(points[2].Y - points[3].Y)
			angle := math.Atan2(dy, dx)
			if angle < 0 {
				angle += 2 * math.Pi
			}
			degrees := angle * 180 / math.Pi

			projected := dstPoints.ToPoints()
			scm_sendPage(int(d.id), projected[3], projected[2], projected[0], projected[1], degrees)
		}

		gocv.Rectangle(&projection, image.Rect(0, 0, x, y), color.RGBA{255,255,255,255}, 2)
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

