; highlighting means drawing an outline around the tag points
(When ((?someone wishes (?p highlighted ?color))
       (?p (region page-points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((scaling-factor 2.0)
           (mid (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5))))
           (ulhc (vec->ints (vec-add mid (vec-mul (vec-from-to mid ?ulhc) scaling-factor))))
           (urhc (vec->ints (vec-add mid (vec-mul (vec-from-to mid ?urhc) scaling-factor))))
           (llhc (vec->ints (vec-add mid (vec-mul (vec-from-to mid ?llhc) scaling-factor))))
           (lrhc (vec->ints (vec-add mid (vec-mul (vec-from-to mid ?lrhc) scaling-factor)))))
      (draw-outline ?color 2 ulhc urhc llhc lrhc)))

(define (get-color c)
  (case c
    ((red) '(255 0 0))
    ((green) '(0 255 0))
    ((blue) '(0 0 255))
    ((white) '(255 255 255))
    ((black) '(0 0 0))
    (else c))) ; TODO validate len 3 list of R G B values

(define (draw-outline color width ulhc urhc llhc lrhc)
  (let* ((c (get-color color))
         (r (car c)) (g (cadr c)) (b (caddr c)))
  (draw-line projection (car ulhc) (cdr ulhc) (car urhc) (cdr urhc) r g b width)
  (draw-line projection (car urhc) (cdr urhc) (car lrhc) (cdr lrhc) r g b width)
  (draw-line projection (car lrhc) (cdr lrhc) (car llhc) (cdr llhc) r g b width)
  (draw-line projection (car llhc) (cdr llhc) (car ulhc) (cdr ulhc) r g b width)))

(When ((?p (region outline) (?ulhc ?urhc ?llhc ?lrhc)))
 do (draw-outline 'white 2 ?ulhc ?urhc ?llhc ?lrhc))
