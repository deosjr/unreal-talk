(use-modules (system foreign)
             (system foreign-library))

(define lib (load-foreign-library "./libcvmatwrapper.dylib"))

(define create-image
  (foreign-library-function lib "create_image"
                            #:arg-types (list int int int)
                            #:return-type '*))

(define fill-image
  (foreign-library-function lib "fill_image"
                            #:arg-types (list '* uint8 uint8 uint8)
                            #:return-type void))

(define save-image
  (foreign-library-function lib "save_image"
                            #:arg-types (list '* '*)
                            #:return-type int))

(define copy-from-to
  (foreign-library-function lib "copyTo"
                            #:arg-types (list '* '* '*)
                            #:return-type void))

(define free-image
  (foreign-library-function lib "free_image"
                            #:arg-types (list '*)
                            #:return-type void))

(define get-text-size
  (foreign-library-function lib "getTextSize"
                            #:arg-types (list '* int double int '*)
                            #:return-type void))

(define put-text
  (foreign-library-function lib "putText"
                            #:arg-types (list '* '* int int int double int int int int)
                            #:return-type void))

(define draw-line
  (foreign-library-function lib "line"
                            #:arg-types (list '* int int int int int int int int)
                            #:return-type void))

(define draw-rectangle
  (foreign-library-function lib "rectangle"
                            #:arg-types (list '* int int int int int int int int)
                            #:return-type void))

(define fill-poly
  (foreign-library-function lib "fill_poly"
                            #:arg-types (list '* '* int int int int)
                            #:return-type void))

(define point-polygon-test
  (foreign-library-function lib "point_polygon_test"
                            #:arg-types (list '* int int int)
                            #:return-type double))

(define rotation-matrix-2d
  (foreign-library-function lib "get_rotation_matrix_2d"
                            #:arg-types (list int int double double)
                            #:return-type '*))

(define warp-affine
  (foreign-library-function lib "warp_affine"
                            #:arg-types (list '* '* '* int int)
                            #:return-type void))
