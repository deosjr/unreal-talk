; This script provides basic support for whiskers (ie pointing at things)
; whisker length == page height

(When ((?p wishes (?p has-whiskers #t))) do
  (Claim ?p 'has-whiskers #t))

; page rotates around midpoint: from there to whisker end, add halfh + whisker length
(When ((?p has-whiskers #t)
       (?p (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((mid (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?urhc) 0.5))))
           (midx (car mid)) (midy (cdr mid))
           (end (vec->ints (vec-add mid (vec-mul (vec-from-to ?lrhc ?urhc) 2))))
           (endx (car end)) (endy (cdr end)))
       (draw-line projection midx midy endx endy 0 255 0 2)
       (Claim ?p 'pointer-at (cons endx endy))))

; Cheap pure-Scheme bbox reject before the FFI polygon test: the endpoint
; can only be inside the quad if it's inside the quad's bounding box, so
; outside-the-bbox pairs (the common case) skip the bytevector alloc +
; pointer marshalling + OpenCV call entirely. bbox is a superset, so this
; never rejects a real hit.
(define (bounding-box-test pts x y)
  (let* ((xs (map car pts)) (ys (map cdr pts)))
    (if (and (>= x (apply min xs)) (<= x (apply max xs))
               (>= y (apply min ys)) (<= y (apply max ys)))
      (let* ((ulhc (car pts)) (urhc (cadr pts))
             (llhc (caddr pts)) (lrhc (cadddr pts))
             (pts (points->bytevector ulhc urhc lrhc llhc))
             (ptr (bytevector->pointer pts))
             (test (point-polygon-test ptr 4 x y)))
        (> test 0))
      #f)))

; NOTE: this fires for every page, since we can't calculate in the db atm!
(When ((?p pointer-at (?px . ?py))
       (?r (region points) ?pts))
  do (if (bounding-box-test ?pts ?px ?py)
         (Claim ?p 'points-at ?r)))

; This is clean, but some other ways of expressing this relation
; tanked the performance drastically. Smth to be mindful of!
(When ((?p points-at ?r)
       (?q (region tag) ?r))
 do (Claim ?p 'points-at ?q))
