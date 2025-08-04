# unreal-talk

Second attempt at building a Dynamicland style environment

## How to run

* install golang and guile scheme
* install opencv and apriltag
* recommended: run webcam driver or use v4l2 to tweak
* `make calibrate` to calibrate camera: needs chessboard fullscreen (f)
* `make run` and fullscreen (f) the projection window on the projector

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

* 02-08-2025: async fetching urls from within Guile Scheme using threads
* 31-07-2025: datalog inefficiency starts to hurt; multiple rules get slow quite fast
* 30-07-2025: First live-edit demo is now working
* 30-07-2025: Using Logitech software to manually set webcam config is a drastic improvement
* 29-07-2025: Rough first implementation of memories using Remember/Forget
* 28-07-2025: Claim-derived as temporary fix for Claim-within-When bugs
* 27-07-2025: Guile openCV bindings are getting painful to write, need better wrappers
* 26-07-2025: Parallelizing tag parsing speeds things up dramatically
* 26-07-2025: Simple logging shows: tags flickering causes increasing lag in Scheme (fixed)
* 25-07-2025: Current setup can recognize 4x4cm tags stably
* 25-07-2025: Use the file system as database for page scripts
* 24-07-2025: Claim and When fact order is now in sync
* 23-07-2025: RealTalk ported from Hoot, draw routine triggered from fixpoint
* 22-07-2025: Go sends Guile a cv::Mat pointer, Guile draws on it, Go shows it
* 21-07-2025: Guile Scheme can call opencv through a C++ wrapper
* 21-07-2025: Golang talks to Guile Scheme using CGO directly now
* 21-07-2025: homography working and stable. for 3d look into PnPRansac
* 20-07-2025: gocv FileNode.Mat() is not implemented so we work around it for now
* 20-07-2025: chessboard calibration + hardcoded homography detection
* 19-07-2025: got Go working with gocv for webcam and CGO for apriltags
* 19-07-2025: gave up on FFI from Guile Scheme -> openCV + AprilTags for now
* 19-07-2025: evaluated QR-code detection in openCV, deemed not good enough
