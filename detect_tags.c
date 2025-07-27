#include <stdlib.h>
#include "../apriltag/apriltag.h"
#include "../apriltag/tagStandard41h12.h"
#include <string.h>

typedef struct {
	apriltag_detector_t* td;
} Detector;

Detector* new_detector() {
	apriltag_family_t* tf = tagStandard41h12_create();
	apriltag_detector_t* td = apriltag_detector_create();
	apriltag_detector_add_family(td, tf);

	// optional fine-tuning
	//td->debug = true;
	//td->nthreads = 10;
	//td->quad_decimate = 2.0; // default is 2.0
	//td->decode_sharpening = 0.5;
	//printf("%f\n", td->quad_decimate);
	Detector* d = (Detector*)malloc(sizeof(Detector));
	d->td = td;
	return d;
}

void free_detector(Detector* d) {
	apriltag_detector_destroy(d->td);
	//todo: tagStandard41h12_destroy(d->tf);
}

typedef struct {
	int id;
	float cx, cy;
	float corners[4][2];
} Detection;

Detection* detect_tags(Detector* d, uint8_t* gray, int width, int height, int* num) {
	image_u8_t img = {
		.width = width,
		.height = height,
		.stride = width,
		.buf = gray
	};

	zarray_t* detections = apriltag_detector_detect(d->td, &img);

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

	apriltag_detections_destroy(detections);

	return results;
}
