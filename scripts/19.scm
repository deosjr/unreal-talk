; pong paddle

; paddles self-assign left/right role
(When ((,?pong has-region (pong ,?rotation ,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,?pong scale ,?scale)
       (,this (page points) (,?a ,?b ,?c ,?d)))
 do (let* ((center (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5))))
           (cx (car center)) (cy (cdr center))
; unrotate paddle tag around pong area center, so we can calculate more easily
           (m (rotation-matrix-2d cx cy ?rotation 1.0)) ; counter-clockwise rotation!
           (in-pts (bytevector->pointer (points->bytevector ?a ?b ?c ?d)))
           (out-pts (bytevector->pointer (make-bytevector (* 8 4)))))
      (transform in-pts 4 m out-pts)
      (free-image m)
      (let* ((pts (pts->coords out-pts 4))
             (tulhc (car pts)) (turhc (cadr pts))
             (tllhc (caddr pts)) (tlrhc (cadddr pts))
             (tcenter (vec->ints (vec-add tulhc (vec-mul (vec-from-to tulhc tlrhc) 0.5))))
             (tcx (car tcenter))
             (y (/ (cdr (vec-sub tcenter ?ulhc)) ?scale)))
        (cond
          [(< tcx (car ?ulhc)) (Claim-derived this ?pong 'pong-paddle (cons 'left y))]
          [(> tcx (car ?urhc)) (Claim-derived this ?pong 'pong-paddle (cons 'right y))]
        )
)))
