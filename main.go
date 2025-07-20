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

	window := gocv.NewWindow("AprilTag")
	defer window.Close()

	img := gocv.NewMat()
	defer img.Close()

	fs := gocv.NewFileStorage()
	defer fs.Close()

	data := []float64{
		2.7446652529921467, 0.19799366932629123, -1584.4781742467537,
		-0.0060984174295913626, 2.9426603187621247, -818.4341619951682,
		-2.0331732694873542e-05, 0.0001549515645058243, 1,
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

		// Convert to grayscale
		gray := gocv.NewMat()
		gocv.CvtColor(img, &gray, gocv.ColorBGRToGray)
		defer gray.Close()

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
			pv := gocv.NewPointVectorFromPoints(points)
			rect := gocv.BoundingRect(pv)
			fmt.Println(d.id, center, d.corners)
			gocv.Circle(&img, center, 5, color.RGBA{0, 255, 0, 0}, 2)
			gocv.Rectangle(&img, rect, color.RGBA{0, 255, 0, 0}, 2)
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
			polygons := gocv.NewPointsVector()
			polygons.Append(dstPoints)
			gocv.FillPoly(&img, polygons, color.RGBA{R: 255, A: 255})
			fmt.Println(pv2f.ToPoints())
			fmt.Println(dstPoints.ToPoints())
		}

		window.IMShow(img)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
	}
}

