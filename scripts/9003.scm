; highlighting means drawing an outline around the tag points
(When ((?someone wishes (?p highlighted ?color))
       (?p (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((scaling-factor 2.0)
           (mid (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5))))
           (pts (map (lambda (x)
             (vec->ints (vec-add mid (vec-mul (vec-from-to mid x) scaling-factor))))
             (list ?ulhc ?urhc ?llhc ?lrhc))))
      (draw-outline ?color 2 pts)))

(define (get-color c)
  (case c
    ((red) '(255 0 0))
    ((green) '(0 255 0))
    ((blue) '(0 0 255))
    ((white) '(255 255 255))
    ((black) '(0 0 0))
    (else c))) ; TODO validate len 3 list of R G B values

(define (draw-outline color width pts)
  (let* ((c (get-color color))
         (ulhc (car pts)) (urhc (cadr pts))
         (llhc (caddr pts)) (lrhc (cadddr pts))
         (r (car c)) (g (cadr c)) (b (caddr c)))
  (draw-line projection (car ulhc) (cdr ulhc) (car urhc) (cdr urhc) r g b width)
  (draw-line projection (car urhc) (cdr urhc) (car lrhc) (cdr lrhc) r g b width)
  (draw-line projection (car lrhc) (cdr lrhc) (car llhc) (cdr llhc) r g b width)
  (draw-line projection (car llhc) (cdr llhc) (car ulhc) (cdr ulhc) r g b width)))

; new region format
(When ((?r (region name) outline)
       (?r (region points) ?pts))
 do (draw-outline 'white 2 ?pts))

; old region format, deprecated
(When ((?p (region outline) (?ulhc ?urhc ?llhc ?lrhc)))
 do (draw-outline 'white 2 (list ?ulhc ?urhc ?llhc ?lrhc)))
