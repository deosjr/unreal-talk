package main

import (
	"fmt"
	"image"

	"gocv.io/x/gocv"
)

func print_tag() {
//func main() {
	w, h := 2480, 3508 // 300 ppi/dpi
	// a4 in cm: 21 x 29.7
	// which means 1cm in pixels = 2480/21 =~ 118 (?)
	cm := 118
	img := gocv.NewMatWithSize(h, w, gocv.MatTypeCV8UC1)
	img.SetTo(gocv.NewScalar(255, 255, 255, 255))
	defer img.Close()
	// a tag is 9x9 pixels, so each pixel is now ~1cm
	size := 9*cm

	file := fmt.Sprintf("../apriltag-imgs/tagStandard41h12/tag41_12_%05d.png", 1)
	tag := gocv.IMRead(file, gocv.IMReadGrayScale)
	defer tag.Close()

	resized := gocv.NewMat()
	gocv.Resize(tag, &resized, image.Pt(size, size), 0, 0, gocv.InterpolationNearestNeighbor)

	padding := 2*cm
	r := img.Region(image.Rect(padding, padding, padding+size, padding+size))
	defer r.Close()
	resized.CopyTo(&r)
/*
	maxX, maxY := 3, 4
	for y := 1; y <= maxY; y++ {
		for x := 1; x <= maxX; x++ {
			id := maxX*(y-1) + x
			file := fmt.Sprintf("../apriltag-imgs/tagStandard41h12/tag41_12_%05d.png", id)
			tag := gocv.IMRead(file, gocv.IMReadGrayScale)
			defer tag.Close()
		
			resized := gocv.NewMat()
			gocv.Resize(tag, &resized, image.Pt(size, size), 0, 0, gocv.InterpolationNearestNeighbor)

			startx := 250+(x-1)*(size+100)
			endx := startx+size
			starty := 300+(y-1)*(size+100)
			endy := starty+size
			r := img.Region(image.Rect(startx, starty, endx, endy))
			defer r.Close()
			resized.CopyTo(&r)
		}
	}
*/

/*
	file := "../apriltag-imgs/tagStandard41h12/tag41_12_00012.png"
	tag := gocv.IMRead(file, gocv.IMReadGrayScale)
	defer tag.Close()
	// tag.Size() = [9 9]

	resized := gocv.NewMat()
	x, y := 1000, 300
	for i := 1; i <= 5; i++ {
		size := i*cm
		gocv.Resize(tag, &resized, image.Pt(size, size), 0, 0, gocv.InterpolationNearestNeighbor)
	
		r := img.Region(image.Rect(x, y, x+size, y+size))
		defer r.Close()
		resized.CopyTo(&r)
		y += size + 100
	}
*/

	gocv.IMWrite("test.png", img)
}
