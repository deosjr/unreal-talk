; This script colors the entire projection based on its rotation

(define (fmod x y)
  (- x (* y (floor (/ x y)))))

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
      (fill-image projection (scale r) (scale g) (scale b))))
