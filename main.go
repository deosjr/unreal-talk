package main

// #include "detect_tags.c"
/*
#cgo CFLAGS: -I../apriltag
#cgo LDFLAGS: -L../apriltag/build -lapriltag
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
			pv := gocv.NewPointVectorFromPoints([]image.Point{
				image.Pt(int(d.corners[0][0]), int(d.corners[0][1])),
				image.Pt(int(d.corners[1][0]), int(d.corners[1][1])),
				image.Pt(int(d.corners[2][0]), int(d.corners[2][1])),
				image.Pt(int(d.corners[3][0]), int(d.corners[3][1])),
			})
			rect := gocv.BoundingRect(pv)
			fmt.Println(d.id, center, d.corners)
			gocv.Circle(&img, center, 5, color.RGBA{0, 255, 0, 0}, 2)
			gocv.Rectangle(&img, rect, color.RGBA{0, 255, 0, 0}, 2)
			gocv.PutText(&img, fmt.Sprintf("ID %d", d.id), center, gocv.FontHersheyPlain, 1.2, color.RGBA{255, 0, 0, 0}, 2)
		}

		window.IMShow(img)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
	}
}

