package main

import (
	"fmt"
	"image"
	"image/color"

	"gocv.io/x/gocv"
)

// numCols/Rows is amount of squares
// for detection the intersection points matter (-1)
func generateChessboard(squareSize, numCols, numRows int) gocv.Mat {
	width := numCols * squareSize
	height := numRows * squareSize
	img := gocv.NewMatWithSize(height, width, gocv.MatTypeCV8UC1)
	white := color.RGBA{255, 255, 255, 0}
	for row := 0; row < numRows; row++ {
		for col := 0; col < numCols; col++ {
			if (row+col)%2 == 0 {
				start := image.Pt(col*squareSize, row*squareSize)
				end := image.Pt((col+1)*squareSize-1, (row+1)*squareSize-1)
				gocv.Rectangle(&img, image.Rect(start.X, start.Y, end.X, end.Y), white, -1)
			}
		}
	}
	return img
}

func calibrate() {
	webcam, _ := gocv.OpenVideoCapture(0)
	defer webcam.Close()

	window := gocv.NewWindow("Debug")
	defer window.Close()
	img := gocv.NewMat()
	defer img.Close()

	chesswindow := gocv.NewWindow("Chessboard")
	defer chesswindow.Close()

	// 1280x720 projection with aspect ratio 16:9
	squareSize := 80
	cols, rows := 16, 9
	patternSize := image.Pt(cols-1, rows-1) // inner corners
	chesswindow.IMShow(generateChessboard(squareSize, cols, rows))

	var projectorPoints []gocv.Point2f
	for y := 1; y <= patternSize.Y; y++ {
	    for x := 1; x <= patternSize.X; x++ {
    	    	projectorPoints = append(projectorPoints, gocv.Point2f{
 	   		X: float32(x*squareSize),
 	   		Y: float32(y*squareSize),
 	   	})
	    }
	}

	for {
		if window.WaitKey(1) == 27 || chesswindow.WaitKey(1) == 27 {
			break // ESC to start calibrating: gives time to put chessboard in place
		}
	}
    	chesswindow.SetWindowProperty(gocv.WindowPropertyFullscreen, gocv.WindowFullscreen)

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

		window.IMShow(gray)
		if window.WaitKey(1) == 27 {
			break // ESC to quit
		}
		
		corners := gocv.NewMat()
		defer corners.Close()
		flags := gocv.CalibCBAdaptiveThresh | gocv.CalibCBExhaustive
		found := gocv.FindChessboardCorners(gray, patternSize, &corners, flags)
		fmt.Println("Corners found:", found, " count:", corners.Total())
		if !found {
			fmt.Println("Chessboard not found")
			continue
		}
		gocv.CornerSubPix(gray, &corners, image.Pt(11, 11), image.Pt(-1, -1),
			gocv.NewTermCriteria(gocv.MaxIter|gocv.EPS, 30, 0.001))
		mask := gocv.NewMat()
		defer mask.Close()
		points := gocv.NewMatFromPoint2fVector(gocv.NewPoint2fVectorFromPoints(projectorPoints), true)
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
		break

/*
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

*/
	}
	for i := 0; i<3; i++ {
		for j := 0; j<3; j++ {
			fmt.Println(homography.GetDoubleAt(i, j))
		}
	}
}
