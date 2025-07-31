package main

//#include <libguile.h>
import "C"
import (
	"image"
	"unsafe"

	"gocv.io/x/gocv"
)

func scm_point(p image.Point) C.SCM {
	return C.scm_cons(C.scm_from_int(C.int(p.X)), C.scm_from_int(C.int(p.Y)))
}

func scm_pageGeometry(pg pageGeometry) C.SCM {
	id := C.scm_from_int(C.int(pg.id))
	ulhc :=  scm_point(pg.ulhc)
	urhc :=  scm_point(pg.urhc)
	llhc :=  scm_point(pg.llhc)
	lrhc :=  scm_point(pg.lrhc)
	points := C.scm_list_4(ulhc, urhc, llhc, lrhc)
	rotation := C.scm_from_double(C.double(pg.rotation))
	return C.scm_list_3(id, points, rotation)
}

func scm_sendKeyDown(key int) {
	fname := C.CString("receive-key-down")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_1(f, C.scm_from_int(C.int(key)))
}

func scm_sendTimeDetect(d int64) {
	fname := C.CString("receive-time-detect")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_1(f, C.scm_from_int(C.int(d)))
}

func scm_sendTimeSCM(d int64) {
	fname := C.CString("receive-time-scm")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_1(f, C.scm_from_int(C.int(d)))
}

// this function triggers dl-fixpoint!
func scm_sendPageGeometries(geos []pageGeometry) {
	fname := C.CString("receive-pages-found")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	list := C.SCM_EOL
	for i:=len(geos)-1; i >=0; i-- {
		pg := scm_pageGeometry(geos[i])
		list = C.scm_cons(pg, list)
	}
	C.scm_call_1(f, list)
}

func scm_sendImageInfo(img, proj, m gocv.Mat, x, y int) {
	imgptr := unsafe.Pointer(img.Ptr())
	projptr := unsafe.Pointer(proj.Ptr())
	mptr := unsafe.Pointer(m.Ptr())
	fname := C.CString("init-images")
	defer C.free(unsafe.Pointer(fname))
	f := C.scm_variable_ref(C.scm_c_lookup(fname))
	C.scm_call_5(f, C.scm_from_pointer(imgptr, nil),
                        C.scm_from_pointer(projptr, nil),
                        C.scm_from_pointer(mptr, nil),
                        C.scm_from_int(C.int(x)),
                        C.scm_from_int(C.int(y)))
}
