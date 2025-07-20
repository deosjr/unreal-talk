package main

import (
	"fmt"
	"image"
	"image/color"

	"gocv.io/x/gocv"
)

func main() {
	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()

	window := gocv.NewWindow("AprilTag")
	defer window.Close()

	img := gocv.NewMat()
	defer img.Close()

	patternSize := image.Pt(9, 6) // inner corners
	squareSize := 200 // pixels
	var projectorPoints []gocv.Point2f
    	for y := 0; y < patternSize.Y; y++ {
    	    for x := 0; x < patternSize.X; x++ {
		projectorPoints = append(projectorPoints, gocv.Point2f{
			X: float32(x*squareSize),
			Y: float32(y*squareSize),
		})
    	    }
    	}

	points := gocv.NewMatFromPoint2fVector(gocv.NewPoint2fVectorFromPoints(projectorPoints), true)
	var homography gocv.Mat
	for {
		if ok := webcam.Read(&img); !ok || img.Empty() {
			continue
		}
		gray := gocv.NewMat()
		defer gray.Close()
		gocv.CvtColor(img, &gray, gocv.ColorBGRToGray)
		gocv.GaussianBlur(gray, &gray, image.Pt(5, 5), 0, 0, gocv.BorderDefault)
		gocv.Threshold(gray, &gray, 160, 255, gocv.ThresholdBinaryInv)
		//gray.ConvertToWithParams(&gray, gocv.MatTypeCV8U, 2.0, -250)
		
		corners := gocv.NewMat()
		defer corners.Close()
		flags := gocv.CalibCBAdaptiveThresh | gocv.CalibCBExhaustive
		found := gocv.FindChessboardCorners(gray, patternSize, &corners, flags)
		fmt.Println("Corners found:", found, " count:", corners.Total())
		if !found {
			fmt.Println("Chessboard not found")
			continue
		}
		//gocv.CornerSubPix(gray, &corners, image.Pt(11, 11), image.Pt(-1, -1),
			//gocv.NewTermCriteria(gocv.Count|gocv.EPS, 30, 0.1))
		mask := gocv.NewMat()
		defer mask.Close()
		fmt.Println(gocv.NewPoint2fVectorFromMat(corners).ToPoints())
		fmt.Println(gocv.NewPoint2fVectorFromMat(points).ToPoints())
		fmt.Println(points.Total())
		h := gocv.FindHomography(
			corners,
			points,
			gocv.HomographyMethodRANSAC,
			3.0, 	// reprojection threshold
			&mask, 	// output mask
			2000,	// max iterations,
			0.995,	// confidence
		)
		fmt.Println("Homography matrix:\n", h)
		homography = h
		//break

		pts := gocv.NewPoint2fVectorFromMat(corners).ToPoints()
		for _, p := range pts {
			gocv.Circle(&img, image.Pt(int(p.X), int(p.Y)), 5, color.RGBA{255, 0, 0, 0}, 2)
		}
		for _, p := range projectorPoints {
			gocv.Circle(&img, image.Pt(int(p.X), int(p.Y)), 5, color.RGBA{0, 255, 0, 0}, 2)
		}
		for i := 0; i<3; i++ {
			for j := 0; j<3; j++ {
				fmt.Println(homography.GetDoubleAt(i, j))
			}
		}

		window.IMShow(img)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
	}
	/*
	// would be nice, but gocv doesnt implement FileNode.Mat() ...
	fs := gocv.NewFileStorage()
	defer fs.Close()
	
	ok := fs.Open("homography.yml", gocv.FileStorageModeWrite, "utf-8")
	if !ok {
		panic("Failed to open file for writing")
	}
	
	fs.WriteMat("homography", homography)
	//fs.Release()
	*/
}
