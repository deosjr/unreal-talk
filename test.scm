(use-modules (system foreign-library)
             (system foreign))

(define webcam-lib (load-foreign-library "./webcam.dylib"))

(define webcam-capture (foreign-library-function "webcam" "capture_webcam_frame" #:arg-types (list '*)))
(webcam-capture (string->pointer "tmp.jpg"))

(define apriltag-lib (load-foreign-library "./libapriltag.dylib"))

(define detector-create (foreign-library-function "libapriltag" "apriltag_detector_create"))

(define detector (detector-create))

(display "Detector pointer: ") (display detector) (newline)
