package main

import (
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

	gocv.IMWrite("test.png", img)
}
