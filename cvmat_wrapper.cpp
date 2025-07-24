// cvmat_wrapper.cpp
#include <opencv2/opencv.hpp>
#include <string>

extern "C" {

// this wrapper needed because Guile has C bindings, not C++
typedef struct {
    cv::Mat mat;
} Image;

Image* create_image(int width, int height, int imgType) {
    Image* img = new Image;
    img->mat = cv::Mat(height, width, imgType, cv::Scalar(0));
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

void copyTo(Image* src, Image* dst, Image* mask) {
    src->mat.copyTo(dst->mat, mask->mat);
}

void free_image(Image* img) {
    if (img != nullptr) {
	delete img;
    }
}

typedef struct {
    int width;
    int height;
    int baseline;
} TextFormat;

void getTextSize(const char* text, int fontFace, double fontScale, int thickness, TextFormat* out) {
    int baseline=0;
    cv::Size size = cv::getTextSize(text, fontFace, fontScale, thickness, &baseline);
    *out = TextFormat{size.width, size.height, baseline};
}

void putText(Image* img, const char* text, int x, int y, int fontFace, double fontScale, int r, int g, int b, int thickness) {
    cv::putText(img->mat, text, cv::Point(x, y), fontFace, fontScale, cv::Scalar(b, g, r), thickness, cv::LINE_8, false);
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

Image* get_rotation_matrix_2d(int cx, int cy, double angle, double scale) {
    Image* mat = new Image;
    cv::Point2f center(cx, cy);
    mat->mat = cv::getRotationMatrix2D(center, angle, scale);
    return mat;
}

void warp_affine(Image* src, Image* dst, Image* transform, int width, int height) {
    cv::warpAffine(
        src->mat,
        dst->mat,
        transform->mat,
        cv::Size(width, height),
        cv::INTER_LINEAR,
        cv::BORDER_CONSTANT,
        cv::Scalar(0)
    );
}

}
