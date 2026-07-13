(define (center ulhc lrhc)
  (vec-add (vec-mul (vec-from-to ulhc lrhc) 0.5) ulhc))

; todo: integer?
(Forget 'world-radius) ; this lets us revert to default 
(Wish this 'has-parameter '(world-radius 500 800 500))

(define pi 3.14159)

; start giant on top of the world
(Remember 'giant-angle (* pi -0.5)) ; why minus?
(Remember 'angular-velocity 0.02)

(When ((time now ?t)
       (this giant-angle ?theta)
       (this angular-velocity ?omega))
 do (Wish this 'titled "REUS")
    (Remember 'giant-angle (+ ?theta ?omega)))

(When ((this (region tag) ?reg)
       (?reg (region points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (Remember 'world-center (center ?ulhc ?lrhc)))

; TODO: move to giant pointer:
; pointer intersects with world at angle
; if giant-angle is not that angle, then
; Remember new angle (but dont overshoot)
(When ((?someone wishes (giant moves-to (?x . ?y)))
       (this world-center ?c))
;do (let ((target-angle (
 do (Wish this 'subtitled (format #f "~a" ?y)))

(define (giant-coords c r theta)
  (let* ((gx (cos theta))
         (gy (sin theta))
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
     (draw-circle projection cx cy r 255 255 255 1)
     (draw-circle projection gx gy 30 0 255 0 -1)))
