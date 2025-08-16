; claim a relative region based on detected tag dimensions
; all calculations are done based on the detected upper left-hand corner
; using inner tag box vectors

(define (region-from-tag dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 ulhc urhc llhc lrhc)
 (let* ((dx-vec (vec-from-to ulhc urhc))
        (dy-vec (vec-from-to ulhc llhc))
        (new-ulhc   (vec->ints (vec-add (vec-add ulhc (vec-mul dx-vec dx1))
                                                      (vec-mul dy-vec dy1))))
        (new-urhc   (vec->ints (vec-add (vec-add ulhc (vec-mul dx-vec dx2))
                                                      (vec-mul dy-vec dy2))))
        (new-llhc   (vec->ints (vec-add (vec-add ulhc (vec-mul dx-vec dx3))
                                                      (vec-mul dy-vec dy3))))
        (new-lrhc   (vec->ints (vec-add (vec-add ulhc (vec-mul dx-vec dx4))
                                                      (vec-mul dy-vec dy4)))))
    (list new-ulhc new-urhc new-llhc new-lrhc)))

(When ((,?someone wishes (,?p has-region-from-tag
  (,?region ,?dx1 ,?dy1 ,?dx2 ,?dy2 ,?dx3 ,?dy3 ,?dx4 ,?dy4)))
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((region (region-from-tag ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4
                                   ?ulhc ?urhc ?llhc ?lrhc)))
      (Claim-derived this ?p 'has-region (cons ?region region))))

(When ((,?someone wishes (,?p has-region-from-tag-unrotated
  (,?region ,?dx1 ,?dy1 ,?dx2 ,?dy2 ,?dx3 ,?dy3 ,?dx4 ,?dy4)))
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,?p (page rotation) ,?rotation)) ; clockwise rotation
 do (let* ((region (region-from-tag ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4
                                    ?ulhc ?urhc ?llhc ?lrhc))
           (ulhc (car region)) (urhc (cadr region))
           (llhc (caddr region)) (lrhc (cadddr region))
           (aabb-pts (rotate-rect ulhc urhc llhc lrhc ?rotation)))
        (Claim-derived this ?p 'has-region (cons ?region aabb-pts))))
