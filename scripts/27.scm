(define (center ulhc lrhc)
  (vec-add (vec-mul (vec-from-to ulhc lrhc) 0.5) ulhc))

; todo: integer?
(Wish this 'has-parameter '(world-radius 100 300 200))

(define pi 3.14159)

; start giant on top of the world
(define giant-rad (* pi 0.5))

(When ((time now ?t))
 do (Wish this 'titled "REUS")
 ;   (Wish this 'subtitled (format #f "~a" (cos pi))))
)

(define (giant-on-world c r)
  (let* ((gx (cos giant-rad))
         (gy (sin giant-rad))
         (unit (cons gx gy)))
       (vec-add (vec-mul unit r) c)))

; TODO: a draw-circle wish that takes a vec
(When ((this (region tag) ?reg)
       (this world-radius ?r)
       (?reg (region points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((c (center ?ulhc ?lrhc))
           (ci (vec->ints c))
           (cx (car ci)) (cy (cdr ci))
           (r (inexact->exact (round ?r)))
           (g (giant-on-world c r))
           (gi (vec->ints g))
           (gx (car gi)) (gy (cdr gi)))
     (draw-circle projection cx cy r 255 255 255 1)
     (draw-circle projection gx gy 10 0 255 0 -1)))
