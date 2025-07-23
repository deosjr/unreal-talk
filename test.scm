(include "realtalk.scm")
(use-modules (system foreign)
             (system foreign-library)
             (rnrs bytevectors))

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

(define fill-poly
  (foreign-library-function lib "fill_poly"
                            #:arg-types (list '* '* int int int int)
                            #:return-type void))


; this img is a pointer to a cv::Mat that is sent by Golang
(define img #f)
(define (init-image ptr)
  (set! img ptr))

(define page4proc (make-page-code
  (Claim this 'highlighted '(0 0 255))
  (When ((highlighted ,?p (,?b ,?g ,?r))
         ((page points) ,?p (,?ulhc ,?urhc ,?llhc ,?lrhc)))
   do (draw-on-page ?ulhc ?urhc ?llhc ?lrhc ?b ?g ?r))
))

(define page12proc (make-page-code
  (Claim this 'highlighted '(255 0 0))
))

;(dl-assert! (get-dl) 4 '(page code) page4proc)
(page4proc 4)
(page4proc 12)

(define *pages-in-scene* (make-hash-table))

(define (pages-found pages)
  (fill-image img 0 0 0) ;; fill black
  (for-each (lambda (page)
    (let ((id (car page))
          (points (cadr page))
          (rotation (caddr page)))
      (update-page-geometry id points rotation))) pages)
  (dl-fixpoint! (get-dl)))

(define (draw-on-page ulhc urhc llhc lrhc b g r)
  (let* ((pts (make-bytevector (* 8 4)))
         (_ (begin
           (bytevector-s32-native-set! pts 0 (car ulhc))
           (bytevector-s32-native-set! pts 4 (cdr ulhc))
           (bytevector-s32-native-set! pts 8 (car urhc))
           (bytevector-s32-native-set! pts 12 (cdr urhc))
           (bytevector-s32-native-set! pts 16 (car lrhc))
           (bytevector-s32-native-set! pts 20 (cdr lrhc))
           (bytevector-s32-native-set! pts 24 (car llhc))
           (bytevector-s32-native-set! pts 28 (cdr llhc)))))
    (fill-poly img (bytevector->pointer pts) 4 b g r)))
