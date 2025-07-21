#ifndef DETECT_TAGS_H
#define DETECT_TAGS_H

#include <stdint.h>

typedef struct {
	int id;
	float cx, cy;
	float corners[4][2];
} Detection;

Detection* detect_tags(uint8_t* gray, int width, int height, int* num);

#endif
