(define (claim-region ulhc urhc llhc lrhc)
  (let ((region (list ulhc urhc llhc lrhc)))
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,this has-region ,region)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,this has-region ,region) #t)
    (Claim this 'has-region region)))

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((diagonal (vec-from-to ?lrhc ?ulhc))
           ; inner dimensions of 9x9 tag at 1cm per pixel: 5x5cm
           ; diagonal is sqrt(50) and to page topleft is sqrt(32)
           ; roughly 7.07 : 5.66
           (topleft (vec-add ?ulhc (vec-mul diagonal (/ 5.66 7.07))))
           ; a4 in cm: 21 x 29.7
           (rightvec (vec-mul (vec-from-to ?ulhc ?urhc) (/ 21 5)))
           (downvec (vec-mul (vec-from-to ?ulhc ?llhc) (/ 29.7 5)))
           (topright (vec-add topleft rightvec))
           (bottomleft (vec-add topleft downvec))
           (bottomright (vec-add bottomleft rightvec))
           (tlx (inexact->exact (round (car topleft)))) (tly (inexact->exact (round (cdr topleft)))) 
           (trx (inexact->exact (round (car topright)))) (try (inexact->exact (round (cdr topright)))) 
           (blx (inexact->exact (round (car bottomleft)))) (bly (inexact->exact (round (cdr bottomleft)))) 
           (brx (inexact->exact (round (car bottomright)))) (bry (inexact->exact (round (cdr bottomright)))))
      (draw-line projection tlx tly trx try 255 255 255 2)
      (draw-line projection trx try brx bry 255 255 255 2)
      (draw-line projection brx bry blx bly 255 255 255 2)
      (draw-line projection blx bly tlx tly 255 255 255 2)
      (claim-region (cons tlx tly) (cons trx try) (cons blx bly) (cons brx bry))))

(When ((,this has-region (,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,this (page rotation) ,?rotation)) ; clockwise rotation
 do (let* ((center (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5)))
           (cx (inexact->exact (round (car center)))) (cy (inexact->exact (round (cdr center))))
           ; m rotates back to axis-aligned with ulhc at upper left hand corner
           (m (rotation-matrix-2d cx cy ?rotation 1.0)) ; counter-clockwise rotation!
           (minv (rotation-matrix-2d cx cy (- ?rotation) 1.0))
           (img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
           (mask (create-image 1280 720 0)) ; 0 is 1-channel CV8U
           (in-pts (bytevector->pointer (points->bytevector ?ulhc ?urhc ?lrhc ?llhc)))
           (out-pts (bytevector->pointer (make-bytevector (* 8 4)))))
      (transform in-pts 4 m out-pts) ; rotate to axis-aligned
      (fill-poly img out-pts 4 0 0 255)
      (fill-poly mask out-pts 4 255 255 255) ; todo: do we always need mask?
      (warp-affine img img minv 1280 720)    ; rotate back
      (warp-affine mask mask minv 1280 720)  ; rotate back
      (copy-from-to img projection mask)
      (free-image img)
      (free-image mask)
))
