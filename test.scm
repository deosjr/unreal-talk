(use-modules (system foreign)
             (system foreign-library))

(define lib (load-foreign-library "./libcvmatwrapper.dylib"))

(define create-image
  (foreign-library-function lib "create_image"
                            #:arg-types (list int int)
                            #:return-type '*))

(define fill-image
  (foreign-library-function lib "fill_image"
                            #:arg-types (list '* uint8 uint8 uint8)
                            #:return-type void))

(define save-image
  (foreign-library-function lib "save_image"
                            #:arg-types (list '* '*)
                            #:return-type int))

(define free-image
  (foreign-library-function lib "free_image"
                            #:arg-types (list '*)
                            #:return-type void))

(define (page-found id ulhc urhc llhc lrhc rotation)
  (display (format #f "~a: ~a ~a ~a ~a ~a" id ulhc urhc llhc lrhc rotation))
  (newline))

; note: not garbage collected, allocated in C!
(define filename (string->pointer "green-output.png"))

(define img (create-image 640 480))
(fill-image img 0 255 0) ;; fill green
(save-image filename img)
(free-image img)
