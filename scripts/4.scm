(Claim this 'hand 'left)

(define (claim-selection-rect p q)
  (let ((id (gensym))
        (rect (cons p q)))
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,id selection-rect ,rect)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,id selection-rect ,rect) #t)
    (Claim id 'selection-rect rect)))

(When ((,?p hand right)
       (,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,?p (page points) (,?pulhc ,?purhc ,?pllhc ,?plrhc)))
 do (let* ((left-mid (vec-add ?ulhc (vec-mul (vec-from-to ?lrhc ?ulhc) (/ 2 5))))
           (left-midx (inexact->exact (round (car left-mid)))) (left-midy (inexact->exact (round (cdr left-mid))))
           (right-mid (vec-add ?pulhc (vec-mul (vec-from-to ?plrhc ?pulhc) (/ 2 5))))
           (right-midx (inexact->exact (round (car right-mid)))) (right-midy (inexact->exact (round (cdr right-mid)))) )
      (draw-rectangle projection left-midx left-midy right-midx right-midy 255 255 255 2)
      (claim-selection-rect (cons left-midx left-midy) (cons right-midx right-midy))
))

; take region p-q in webcam space
; resize webcam region to projection space based on p-q size
; take a different p-q sized region in projection space
; copy resized webcam region to that region (without mask?)
; NOTE: region in webcam space not guaranteed to be axis-aligned anymore!
(When ((,?id selection-rect (,?p . ,?q))
       (,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((proj-pts (bytevector->pointer (points->bytevector-2 ?p ?q)))
          (out-pts (bytevector->pointer (make-bytevector (* 8 2)))))
      (perspective-transform proj-pts 2 projection->webcam out-pts)
      (let* ((projection-rect (bounding-rect proj-pts 2))
             (pw (rect-width projection-rect))
             (ph (rect-height projection-rect))
             (webcam-rect (bounding-rect out-pts 2))
             (webcam-roi (region-from-rect webcam webcam-rect))
             (dest (region projection 0 0 pw ph))
             (mask (create-image pw ph 0))
             (roi-scaled (create-image pw ph 16)))
        (fill-image mask 255 255 255)
        (resize webcam-roi roi-scaled pw ph 0 0 3) ; cv::INTER_AREA=3
        (copy-from-to roi-scaled dest mask)
        (free-image webcam-roi)
        (free-image dest)
        (free-image mask)
        (free-image roi-scaled)
        (free-rectangle projection-rect)
        (free-rectangle webcam-rect))))




