(include "opencv.scm")
(include "realtalk.scm")
(use-modules (rnrs bytevectors))

(define (text-size str font scale thickness)
  (let ((buf (bytevector->pointer (make-bytevector (* 3 (sizeof int)))))
         (cstr (string->pointer str)))
    (get-text-size cstr font scale thickness buf)
    (parse-c-struct buf (list int int int))))

(display (text-size "hello" 0 2.0 3))

(define (points->bytevector a b c d)
  (let* ((pts (make-bytevector (* 8 4)))
         (_ (begin
           (bytevector-s32-native-set! pts 0 (car a))
           (bytevector-s32-native-set! pts 4 (cdr a))
           (bytevector-s32-native-set! pts 8 (car b))
           (bytevector-s32-native-set! pts 12 (cdr b))
           (bytevector-s32-native-set! pts 16 (car c))
           (bytevector-s32-native-set! pts 20 (cdr c))
           (bytevector-s32-native-set! pts 24 (car d))
           (bytevector-s32-native-set! pts 28 (cdr d))))) pts))

(define (draw-on-page ulhc urhc llhc lrhc r g b)
  (fill-poly img (bytevector->pointer (points->bytevector ulhc urhc llhc lrhc)) 4 r g b))

(define (vec-add p q)
  (let ((px (car p)) (py (cdr p))
        (qx (car q)) (qy (cdr q)))
    (cons (+ px qx) (+ py qy))))

(define (vec-sub p q)
  (let ((px (car p)) (py (cdr p))
        (qx (car q)) (qy (cdr q)))
    (cons (- px qx) (- py qy))))

(define (vec-mul p scalar)
  (let ((px (car p)) (py (cdr p)))
    (cons (* px scalar) (* py scalar))))

(define (vec-from-to p q) (vec-sub q p))

(define page4proc (make-page-code
  (Wish this 'has-whiskers #t)
  (When ((,this points-at ,?p)
         (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
   do (draw-on-page ?ulhc ?urhc ?lrhc ?llhc 0 255 0))))
(dl-assert! (get-dl) 4 '(page code) page4proc)
(hash-set! *procs* 4 page4proc)

; whisker length == page height
(define page12proc (make-page-code
  (define (claim-has-whiskers p)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p has-whiskers #t)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p has-whiskers #t) #t)
    (Claim p 'has-whiskers #t))
  (define (claim-pointer-at p point)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p pointer-at ,point)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p pointer-at ,point) #t)
    (Claim p 'pointer-at point))
  (define (claim-point-at p q)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))

  (When ((,?p wishes (,?p has-whiskers ,#t))) do
    (claim-has-whiskers ?p))

  ; page rotates around midpoint: from there to whisker end, add halfh + whisker length
  ; all of this logic becomes easier here if we do some of the calculations in modules/realtalk
  (When ((,?p has-whiskers #t)
         (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
   do (let* ((upvec (vec-from-to ?ulhc ?urhc))
             (mid (vec-add ?ulhc (vec-mul upvec 0.5)))
             (midx (inexact->exact (round (car mid)))) (midy (inexact->exact (round (cdr mid))))
             (end (vec-add mid (vec-mul (vec-from-to ?lrhc ?urhc) 2)))
             (endx (inexact->exact (round (car end)))) (endy (inexact->exact (round (cdr end)))))
         (draw-line img midx midy endx endy 0 255 0 2)
         (claim-pointer-at ?p (cons endx endy))))

  ; NOTE: this fires for every page, since we can't calculate in the db atm!
  ; todo: rotated page now checks bounding box, not actual div dimensions
  (When ((,?p pointer-at (,?px . ,?py))
         (,?q (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
    do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
              (ptr (bytevector->pointer pts))
              (test (point-polygon-test ptr 4 ?px ?py)))
      (if (> test 0) (claim-point-at ?p ?q))))
))
(dl-assert! (get-dl) 12 '(page code) page12proc)
(hash-set! *procs* 12 page12proc)

#|
(define (fmod x y)
  (- x (* y (floor (/ x y)))))

(define page4proc (make-page-code
  (When ((,this (page rotation) ,?rotation))
   do (let* ((h (/ ?rotation 60.0))
             (c 1.0)
             (x (* c (- 1 (abs (- (fmod h 2) 1)))))
             (r 0) (g 0) (b 0))
        (cond
          ((< h 1) (set! r c) (set! g x) (set! b 0))
          ((< h 2) (set! r x) (set! g c) (set! b 0))
          ((< h 3) (set! r 0) (set! g c) (set! b x))
          ((< h 4) (set! r 0) (set! g x) (set! b c))
          ((< h 5) (set! r x) (set! g 0) (set! b c))
          ((<= h 6) (set! r c) (set! g 0) (set! b x)))
        (define (scale v) (inexact->exact (round (* 255 v))))
        (fill-image img (scale r) (scale g) (scale b))))))
(dl-assert! (get-dl) 4 '(page code) page4proc)
(hash-set! *procs* 4 page4proc)
|#
