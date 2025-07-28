; this script was made for a 9x9cm tag in the upper lefthand corner of an A4 paper

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((diagonal (vec-from-to ?lrhc ?ulhc))
           ; inner dimensions of 9x9 tag at 1cm per pixel: 5x5cm
           ; diagonal is sqrt(50) and to page topleft is sqrt(32)
           ; roughly 7.07 : 5.66
           (topleft (vec-add ?ulhc (vec-mul diagonal (/ 5.66 7.07))))
           ; a4 in cm: 21 x 29.7
           (inner-right-vec (vec-mul (vec-from-to ?ulhc ?urhc) 0.2)) ; normalized to be 1cm/1px long
           (inner-down-vec (vec-mul (vec-from-to ?ulhc ?llhc) 0.2)) ; normalized to be 1cm/1px long
           (rightvec (vec-mul inner-right-vec 21))
           (downvec (vec-mul inner-down-vec 29.7))
           (topright (vec-add topleft rightvec))
           (bottomleft (vec-add topleft downvec))
           (bottomright (vec-add bottomleft rightvec))
           (tlx (inexact->exact (round (car topleft)))) (tly (inexact->exact (round (cdr topleft)))) 
           (trx (inexact->exact (round (car topright)))) (try (inexact->exact (round (cdr topright)))) 
           (blx (inexact->exact (round (car bottomleft)))) (bly (inexact->exact (round (cdr bottomleft)))) 
           (brx (inexact->exact (round (car bottomright)))) (bry (inexact->exact (round (cdr bottomright))))
           (editorulhc (vec-add (vec-add ?llhc (vec-mul inner-right-vec (- 2 ))) (vec-mul inner-down-vec 3)))
           (editorurhc (vec-add editorulhc (vec-mul inner-right-vec 17)))
           (editorlrhc (vec-add editorurhc (vec-mul inner-down-vec 16)))
           (editorllhc (vec-add editorulhc (vec-mul inner-down-vec 16)))
           (etlx (inexact->exact (round (car editorulhc)))) (etly (inexact->exact (round (cdr editorulhc)))) 
           (etrx (inexact->exact (round (car editorurhc)))) (etry (inexact->exact (round (cdr editorurhc))))
           (eblx (inexact->exact (round (car editorllhc)))) (ebly (inexact->exact (round (cdr editorllhc)))) 
           (ebrx (inexact->exact (round (car editorlrhc)))) (ebry (inexact->exact (round (cdr editorlrhc)))))
      (draw-line projection tlx tly trx try 255 255 255 2)
      (draw-line projection trx try brx bry 255 255 255 2)
      (draw-line projection brx bry blx bly 255 255 255 2)
      (draw-line projection blx bly tlx tly 255 255 255 2)
      (Claim-derived this this 'has-region (list (cons etlx etly) (cons etrx etry) (cons eblx ebly) (cons ebrx ebry)))))

(When ((,this has-region (,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,this (page rotation) ,?rotation) ; clockwise rotation
       (,this (page code) ,?str))
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
      (free-image m)
      (free-image minv)
      (Wish-derived this this 'labeled ?str)
))
