// cvmat_wrapper.cpp
#include <opencv2/opencv.hpp>
#include <string>

extern "C" {

// this wrapper needed because Guile has C bindings, not C++
typedef struct {
    cv::Mat mat;
} Image;

Image* create_image(int width, int height) {
    Image* img = new Image;
    img->mat = cv::Mat(height, width, CV_8UC3);
    return img;
}

void fill_image(Image* img, unsigned char r, unsigned char g, unsigned char b) {
    if (img == nullptr) return;
    img->mat = cv::Scalar(b, g, r);  // Note BGR order
}

int save_image(const char* filename, Image* img) {
    if (img == nullptr) return 0;
    return cv::imwrite(filename, img->mat) ? 1 : 0;
}

void free_image(Image* img) {
    if (img != nullptr) {
	delete img;
    }
}

// Fill a polygon defined by points [(x0,y0), (x1,y1), ...]
void fill_poly(Image* img, const int* pts, int npts, int b, int g, int r) {
    std::vector<cv::Point> points;
    for (int i = 0; i < npts; i++) {
	int x = pts[2*i];
	int y = pts[2*i + 1];
	points.push_back(cv::Point(x, y));
    }
    std::vector<std::vector<cv::Point>> polygons = { points };
    cv::fillPoly(img->mat, polygons, cv::Scalar(b, g, r));
}

}

