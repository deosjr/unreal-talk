package main

import (
	"fmt"
	"image"

	"gocv.io/x/gocv"
)

func main() {
	webcam, err := gocv.OpenVideoCapture(0)
	if err != nil {
		panic(err)
	}
	defer webcam.Close()

	window := gocv.NewWindow("unreal-talk")
	defer window.Close()

	img := gocv.NewMat()
	defer img.Close()

	detector := gocv.NewQRCodeDetector()
	defer detector.Close()

	for {
		webcam.Read(&img)

		gray := gocv.NewMat()
		defer gray.Close()
		gocv.CvtColor(img, &gray, gocv.ColorBGRToGray)

		//equalized := gocv.NewMat()
		//gocv.EqualizeHist(gray, &equalized)
		
		binary := gocv.NewMat()
		defer binary.Close()
		gocv.Threshold(gray, &binary, 0, 255, gocv.ThresholdBinary|gocv.ThresholdOtsu)

   		 // Sharpening kernel
   		 kernel := gocv.NewMatWithSizeFromScalar(gocv.NewScalar(0, 0, 0, 0), 3, 3, gocv.MatTypeCV64F)
   		 kernel.SetDoubleAt(0, 1, -1)
   		 kernel.SetDoubleAt(1, 0, -1)
   		 kernel.SetDoubleAt(1, 1, 5)
   		 kernel.SetDoubleAt(1, 2, -1)
   		 kernel.SetDoubleAt(2, 1, -1)
   		 defer kernel.Close()

   		 // Apply filter
   		 sharpened := gocv.NewMat()
		gocv.Filter2D(binary, &sharpened, -1, kernel, image.Pt(-1, -1), 0, gocv.BorderDefault)
   		 defer sharpened.Close()
		
		multiBox := gocv.NewMat()
		defer multiBox.Close()

		success := detector.DetectMulti(sharpened, &multiBox)
		fmt.Println(success, multiBox.Rows())
		var row gocv.Mat
		for i:=0; i < multiBox.Rows(); i++ {
			row = multiBox.Row(i)
			pv := gocv.NewPointVectorFromMat(row)
			rect := gocv.BoundingRect(pv)
			qrRegion := img.Region(rect)
			points := gocv.NewMat()
			defer points.Close()
			qr := gocv.NewMat()
			defer qr.Close()
			str := detector.Decode(qrRegion, points, &qr)
			fmt.Println(str)
		}
		window.IMShow(sharpened)
		window.WaitKey(1)
	}
}
