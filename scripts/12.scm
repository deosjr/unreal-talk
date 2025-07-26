; whisker length == page height
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
(When ((,?p pointer-at (,?px . ,?py))
       (,?q (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
  do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
            (ptr (bytevector->pointer pts))
            (test (point-polygon-test ptr 4 ?px ?py)))
    (if (> test 0) (claim-point-at ?p ?q))))
