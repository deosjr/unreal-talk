// webcam_capture.cpp
#include <opencv2/opencv.hpp>
#include <cstdio>

#include "../apriltag/apriltag.h"

extern "C" int capture_webcam_frame(const char* filename) {
    cv::VideoCapture cap(0);  // Open default camera (device 0)
    if (!cap.isOpened()) {
        return 1;  // Error
    }

    cv::Mat frame;
    cap >> frame;  // Capture one frame

    if (frame.empty()) {
        return 2;  // Empty frame
    }

    // Save to file
    if (!cv::imwrite(filename, frame)) {
        return 3;  // Failed to save
    }

    apriltag_detector_t* td = apriltag_detector_create();
    apriltag_detector_destroy(td);

    return 0;  // Success
}
