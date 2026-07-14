(define (center ulhc lrhc)
  (vec-add (vec-mul (vec-from-to ulhc lrhc) 0.5) ulhc))

; todo: integer?
(Forget 'world-radius) ; this lets us revert to default 
(Wish this 'has-parameter '(world-radius 500 800 500))

(define pi 3.14159)

; start giant on top of the world
; y-axis is flipped because img origin is top-left!!!
(Remember 'giant-angle (* pi 0.5))
(Remember 'angular-velocity 0.02)

(When ((time now ?t)
       (this giant-angle ?theta)
       (this angular-velocity ?omega))
 do (Wish this 'titled "REUS")
    ; todo: modulo 2*pi
    (let ((theta (+ ?theta ?omega)))
      (Remember 'giant-angle theta )))

(When ((this (region tag) ?reg)
       (?reg (region points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (Remember 'world-center (center ?ulhc ?lrhc)))

(define (calc-angle c p)
  (let* ((o (vec-sub p c))
         (l (vec-length o))
         (n (vec-mul o (/ 1 l)))
         (x (car n)) (y (- (cdr n))) ; flip y-axis
         )
  ; im sure this can be done more clever
    (cond
      ((and (>= y 0) (>= x 0)) (asin y))
      ((and (>= y 0) (< x 0)) (- pi (asin y)))
      ((and (< y 0) (< x 0)) (+ pi (- (asin y))))
      (else (+ (* 2 pi) (asin y))))))

; pointer intersects with world at angle
; if giant-angle is not that angle, then
; Remember new angle (but dont overshoot)
(When ((?someone wishes (giant moves-to ?p))
       (this world-center ?c))
 do (let ((target-angle (calc-angle ?c ?p)))
      (Wish this 'subtitled (format #f "~a"
 target-angle))))

(define (giant-coords c r theta)
  (let* ((gx (cos theta))
         (gy (- (sin theta))) ; flip y-axis
         (unit (cons gx gy)))
       (vec-add (vec-mul unit r) c)))

; TODO: a draw-circle wish that takes a vec
(When ((this world-center ?c)
       (this world-radius ?r)
       (this giant-angle ?theta))       
 do (let* ((ci (vec->ints ?c))
           (cx (car ci)) (cy (cdr ci))
           (r (inexact->exact (round ?r)))
           (g (giant-coords ?c r ?theta))
           (gi (vec->ints g))
           (gx (car gi)) (gy (cdr gi)))
;(Wish this 'subtitled (format #f "~a" ?theta))
     (draw-circle projection cx cy r 255 255 255 1)
     (draw-circle projection gx gy 30 0 255 0 -1)))
