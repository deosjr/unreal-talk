# unreal-talk

Second attempt at building a Dynamicland style environment

## How to run

* install golang and guile scheme
* install opencv and apriltag
* calibrate camera by running calibrate.go and having chessboard fullscreen
* todo: manually add homography values to main.go
* run main.go and fullscreen the projection window on the projector

## Log

* 21-07-2025: Golang talks to Guile Scheme using CGO directly now
* 21-07-2025: homography working and stable. for 3d look into PnPRansac
* 20-07-2025: gocv FileNode.Mat() is not implemented so we work around it for now
* 20-07-2025: chessboard calibration + hardcoded homography detection
* 19-07-2025: got Go working with gocv for webcam and CGO for apriltags
* 19-07-2025: gave up on FFI from Guile Scheme -> openCV + AprilTags for now
* 19-07-2025: evaluated QR-code detection in openCV, deemed not good enough
