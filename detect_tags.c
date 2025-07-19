#include <stdlib.h>
#include "../apriltag/apriltag.h"
#include "../apriltag/tagStandard41h12.h"
#include <string.h>

typedef struct {
	int id;
	float cx, cy;
	float corners[4][2];
} Detection;

Detection* detect_tags(uint8_t* gray, int width, int height, int* num) {
	image_u8_t img = {
		.width = width,
		.height = height,
		.stride = width,
		.buf = gray
	};

	apriltag_family_t* tf = tagStandard41h12_create();
	apriltag_detector_t* td = apriltag_detector_create();
	apriltag_detector_add_family(td, tf);

	zarray_t* detections = apriltag_detector_detect(td, &img);

	int count = zarray_size(detections);
	Detection* results = (Detection*) malloc(sizeof(Detection) * count);

	for (int i = 0; i < count; i++) {
		apriltag_detection_t* det;
		zarray_get(detections, i, &det);
		results[i].id = det->id;
		results[i].cx = det->c[0];
		results[i].cy = det->c[1];
		for (int corner = 0; corner < 4; corner++) {
    		    results[i].corners[corner][0] = det->p[corner][0];
    		    results[i].corners[corner][1] = det->p[corner][1];
    		}
	}

	*num = count;

	// todo: reuse detector?
	apriltag_detections_destroy(detections);
	apriltag_detector_destroy(td);
	tagStandard41h12_destroy(tf);

	return results;
}
