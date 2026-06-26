;; rotating dial adjusting a parameter

; only supports floating point params with a min/max
; todo: at least enums, set amount of possible values

(define t_iter 0)
(define prev '())
(define curr '())

(define (check-swap-buffers t)
  (if (not (equal? t t_iter))
    (begin
      (set! prev curr)
      (set! curr '())
      (set! t_iter t))))

(define (center-point ulhc lrhc)
  (let ((diagonal (vec-from-to ulhc lrhc)))
    (vec->ints (vec-add ulhc (vec-mul diagonal 0.5)))))

(When ((?p (region parameter) ?r)
       (?r (region points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let ((center (center-point ?ulhc ?lrhc)))
       (Claim ?r '(region center) center)))

(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc))
       (?r (region name) parameter)
       (?r (region center) ?c)
       (time now ?t))
 do (let* ((center (center-point ?ulhc ?lrhc))
           (d (vec-length (vec-from-to center ?c))))
      (check-swap-buffers ?t)
      (if (null? curr)
        (set! curr (cons d ?r))
        (let ((mind (car curr)))
          (if (< d mind) (set! curr (cons d ?r)))))))

(When ((time now ?t)) do
  (check-swap-buffers ?t)
  (if (not (null? prev))
    (let ((regionid (cdr prev)))
      (Claim this 'adjusts regionid))))

(define param-id #f)

(define (new-id id)
  (let ((new? (not (eqv? id param-id))))
    (if new? (set! param-id id))
    new?))

(define param-min #f)
(define param-max #f)
(define rotation-offset #f)

; We want to create a circle with values from min-max
; and lock in the current orientation of the dial
; to point at the current value of the parameter
; NOTE: we assume a linear space between min/max
(define (lock-in-orientation min max value rotation)
  (set! param-min min)
  (set! param-max max)
  (let* ((adjusted-max (- max min))
         (param-ratio (/ value adjusted-max))
         (rotation-ratio (/ rotation 360))
         ; force positive, although perhaps negative rotation also works
         (offset (* (- (+ 1 rotation-ratio) param-ratio) 360)))
    (set! rotation-offset offset)))

; todo: draw values around the circle
; At rotation offset = 0 we assume the param value = min
; We draw a circle around the dial tag
; such that min AND max are both positioned at the top wrt projection
; this occurs when the param value at time of locking in orientation
; is exactly min OR max (because we work in 360 modulo)
; We can just draw this exact setup unrotated, then rotate according to offset
(define (draw-dial ulhc lrhc)
  (let* ((center (center-point ulhc lrhc))
         (cx (car center)) (cy (cdr center))
         (inner-diagonal (vec-from-to ulhc lrhc))
         (radius (* 1.1 (vec-length inner-diagonal)))
         (rad-int (inexact->exact (round radius))))
    (draw-circle projection cx cy rad-int 255 255 255 2)))

; TODO: draw an arrow to current value?
(define (set-param pid name rotation)
  (Remember pid pid name rotation)) ; TODO

(When ((this adjusts ?r)
       (?p (region parameter) ?r)
       (?r (region parameter) ?paramid)
       (?paramid (parameter name) ?name)
       (?paramid (parameter min) ?min)
       (?paramid (parameter max) ?max)
       ;(?paramid (parameter default) ?default)
       (?p ?name ?value) ; 3 vars, but 2 should be bound!
       (this (page points) (?ulhc ?urhc ?llhc ?lrhc))
       (this (page rotation) ?rotation))
 do (if (new-id ?paramid)
      (lock-in-orientation ?min ?max ?value ?rotation))
    (draw-dial ?ulhc ?lrhc)
    ; todo: draw a line between the dial and the param region?
    (set-param ?p ?name ?rotation))
