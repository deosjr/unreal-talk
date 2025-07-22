# unreal-talk

Second attempt at building a Dynamicland style environment

## How to run

* install golang and guile scheme
* install opencv and apriltag
* calibrate camera by running calibrate.go and having chessboard fullscreen
* todo: manually add homography values to main.go
* run main.go and fullscreen the projection window on the projector

## Design

Golang is the main entry point. It uses opencv to interface with the camera and draw windows.
Go uses apriltags via CGO to detect tags and sends information about them to Guile.
Guile runs realtalk and manages claim/wish/when model.
At start-up time, Go sends an image pointer to Guile for it to draw on.

Each interaction between Go and Guile is one full datalog fixpoint run.
During fixpoint, Guile will draw on the image buffer.
At the end of that interaction, Guile returns from the CGO function call
Go then blits the image to the projection window.

At some point this might be simplified by only using Guile Scheme, but perhaps having Go will be useful when we consider multiple machines runningmultiple realtalk instances.

## Log

* 22-07-2025: Go sends Guile a cv::Mat pointer, Guile draws on it, Go shows it
* 21-07-2025: Guile Scheme can call opencv through a C++ wrapper
* 21-07-2025: Golang talks to Guile Scheme using CGO directly now
* 21-07-2025: homography working and stable. for 3d look into PnPRansac
* 20-07-2025: gocv FileNode.Mat() is not implemented so we work around it for now
* 20-07-2025: chessboard calibration + hardcoded homography detection
* 19-07-2025: got Go working with gocv for webcam and CGO for apriltags
* 19-07-2025: gave up on FFI from Guile Scheme -> openCV + AprilTags for now
* 19-07-2025: evaluated QR-code detection in openCV, deemed not good enough
