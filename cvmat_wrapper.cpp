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

void line(Image* img, int x0, int y0, int x1, int y1, int r, int g, int b, int thickness) {
    cv::Point from(x0, y0);
    cv::Point to(x1, y1);
    cv::line(img->mat, from, to, cv::Scalar(b, g, r), thickness, cv::LINE_8, 0);
}

void rectangle(Image* img, int x0, int y0, int x1, int y1, int r, int g, int b, int thickness) {
    cv::Point from(x0, y0);
    cv::Point to(x1, y1);
    cv::rectangle(img->mat, from, to, cv::Scalar(b, g, r), thickness, cv::LINE_8, 0);
}

// Fill a polygon defined by points [(x0,y0), (x1,y1), ...]
void fill_poly(Image* img, const int* pts, int npts, int r, int g, int b) {
    std::vector<cv::Point> points;
    for (int i = 0; i < npts; i++) {
	int x = pts[2*i];
	int y = pts[2*i + 1];
	points.push_back(cv::Point(x, y));
    }
    std::vector<std::vector<cv::Point>> polygons = { points };
    cv::fillPoly(img->mat, polygons, cv::Scalar(b, g, r));
}

// Wrapper for cv::pointPolygonTest
// Returns:
//   > 0  if the point is inside the polygon
//   = 0  if the point is on the edge
//   < 0  if the point is outside
double point_polygon_test(const int* pts, int npts, int px, int py) {
    std::vector<cv::Point> points;
    for (int i = 0; i < npts; i++) {
	int x = pts[2*i];
	int y = pts[2*i + 1];
	points.push_back(cv::Point(x, y));
    }
    cv::Point pt(px, py);
    return cv::pointPolygonTest(points, pt, false);
}

}

