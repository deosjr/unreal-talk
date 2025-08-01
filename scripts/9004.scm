; claim a relative region based on detected tag dimensions
; all calculations are done based on the detected upper left-hand corner
; using inner tag box vectors

(When ((,?someone wishes (,?p has-region-from-tag
  (,?region ,?dx1 ,?dy1 ,?dx2 ,?dy2 ,?dx3 ,?dy3 ,?dx4 ,?dy4)))
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((dx-vec (vec-from-to ?ulhc ?urhc))
           (dy-vec (vec-from-to ?ulhc ?llhc))
           (ulhc   (vec->ints (vec-add (vec-add ?ulhc (vec-mul dx-vec ?dx1))
                                                      (vec-mul dy-vec ?dy1))))
           (urhc   (vec->ints (vec-add (vec-add ?urhc (vec-mul dx-vec ?dx2))
                                                      (vec-mul dy-vec ?dy2))))
           (llhc   (vec->ints (vec-add (vec-add ?llhc (vec-mul dx-vec ?dx3))
                                                      (vec-mul dy-vec ?dy3))))
           (lrhc   (vec->ints (vec-add (vec-add ?lrhc (vec-mul dx-vec ?dx4))
                                                      (vec-mul dy-vec ?dy4)))))
      (Claim-derived this ?p 'has-region `(,?region ,ulhc ,urhc ,llhc ,lrhc))))
