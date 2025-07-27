#ifndef DETECT_TAGS_H
#define DETECT_TAGS_H

#include <stdint.h>
#include "../apriltag/apriltag.h"
#include "../apriltag/tagStandard41h12.h"

typedef struct {
	apriltag_detector_t* td;
} Detector;

typedef struct {
	int id;
	float cx, cy;
	float corners[4][2];
} Detection;

Detector* new_detector();

void free_detector(Detector* d);

Detection* detect_tags(Detector* d, uint8_t* gray, int width, int height, int* num);

#endif
