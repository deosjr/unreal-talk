// cvmat_wrapper.cpp
#include <opencv2/opencv.hpp>
#include <string>

extern "C" {

typedef void* ImageHandle;

ImageHandle create_image(int width, int height) {
    return new cv::Mat(height, width, CV_8UC3);
}

void fill_image(ImageHandle img, unsigned char r, unsigned char g, unsigned char b) {
    if (img == nullptr) return;
    cv::Mat* mat = static_cast<cv::Mat*>(img);
    *mat = cv::Scalar(b, g, r);  // Note BGR order
}

int save_image(const char* filename, ImageHandle img) {
    if (img == nullptr) return 0;
    cv::Mat* mat = static_cast<cv::Mat*>(img);
    return cv::imwrite(filename, *mat) ? 1 : 0;
}

void free_image(ImageHandle img) {
    if (img != nullptr) {
        delete static_cast<cv::Mat*>(img);
    }
}

}

