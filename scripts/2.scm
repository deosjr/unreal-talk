; This script was made for a 9x9cm tag in the upper lefthand corner of an A4 paper.
; It draws the outline of the A4 paper in white.

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
      (draw-line projection blx bly tlx tly 255 255 255 2)))
