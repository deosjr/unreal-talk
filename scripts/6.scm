; This script provides basic support for whiskers (ie pointing at things)
; whisker length == page height

(When ((,?p wishes (,?p has-whiskers ,#t))) do
  (Claim-derived this ?p 'has-whiskers #t))

; page rotates around midpoint: from there to whisker end, add halfh + whisker length
(When ((,?p has-whiskers #t)
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((mid (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?urhc) 0.5))))
           (midx (car mid)) (midy (cdr mid))
           (end (vec->ints (vec-add mid (vec-mul (vec-from-to ?lrhc ?urhc) 2))))
           (endx (car end)) (endy (cdr end)))
       (draw-line projection midx midy endx endy 0 255 0 2)
       (Claim-derived this ?p 'pointer-at (cons endx endy))))

; NOTE: this fires for every page, since we can't calculate in the db atm!
(When ((,?p pointer-at (,?px . ,?py))
       (,?q (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
  do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
            (ptr (bytevector->pointer pts))
            (test (point-polygon-test ptr 4 ?px ?py)))
    (if (> test 0) (Claim-derived this ?p 'points-at ?q))))
