package main

import (
	"image"
	"image/color"

	"gocv.io/x/gocv"
)

func main() {
	// Number of squares (not inner corners)
	numCols := 10 // Horizontal squares (→ 9 inner corners)
	numRows := 7  // Vertical squares (→ 6 inner corners)
	squareSize := 100 // in pixels

	// Calculate image size
	width := numCols * squareSize
	height := numRows * squareSize

	// Create black image
	img := gocv.NewMatWithSize(height, width, gocv.MatTypeCV8UC1)
	defer img.Close()

	// Fill image with checkerboard pattern
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

	// Save the image
	filename := "chessboard.png"
	ok := gocv.IMWrite(filename, img)
	if ok {
		println("Saved:", filename)
	} else {
		println("Failed to save image.")
	}
}

