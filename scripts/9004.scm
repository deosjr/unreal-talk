; Regions are now attributes: a region named NAME on page ?p is the fact
;   (?p (region NAME) (ulhc urhc llhc lrhc))            ; axis-aligned
;   (?p (region NAME) (rotation ulhc urhc llhc lrhc))   ; rotated
; Page geometry is itself a region: (?p (region page-points) (...)). [done]
;
; A region can be built relative to ANY base region (Phase 2): the wish carries
; the base region's four corners in its value, plus the new region's name and
; direction in the attribute:
;   (Wish this '(region NAME DIR) (list BASE X Y P3 P4))
; where BASE is (ulhc urhc llhc lrhc), DIR is one of right/left/up/down/centered,
; X/Y size the new region in units of BASE's local dx/dy basis vectors, and:
;   - directional dirs: P3 = dist (gap from base centre), P4 = side-offset
;   - centered:         P3 = dx,   P4 = dy   (offset of new ulhc from base centre)
; A consumer fetches a base region first, e.g.
;   (When ((this (region page-points) (?ul ?ur ?ll ?lr)))
;    do (Wish this '(region editor right) (list (list ?ul ?ur ?ll ?lr) 5 5 0.5 0)))
; Because BASE is just four corners, regions can now be built relative to other
; regions, not only the page's own points.
; TODO (Phase 4): pointing at regions (see whiskers).

; Build a region relative to a base region given by its four corners.
; x and y are scaling factors in units of the base's inner dx/dy basis vectors.
; Returns the new region as an int-rounded (ulhc urhc llhc lrhc) list.
(define (region-relative-to dir ulhc urhc llhc lrhc x y p3 p4)
  (let* ((dx (/ x 2.0)) (dy (/ y 2.0))
         (diagonal (vec-from-to ulhc lrhc))
         (mid (vec-add ulhc (vec-mul diagonal 0.5))))
    (define (finish a b c d) (map vec->ints (list a b c d)))
    (cond
      ((eq? dir 'right)
       (let* ((dx-vec (vec-from-to ulhc urhc))
              (dy-vec (vec-from-to ulhc llhc))
              (mid-dir (vec-add mid (vec-mul dx-vec (+ 1 p3))))
              (mid-off (vec-add mid-dir (vec-mul dy-vec p4)))
              (a (vec-add mid-off (vec-mul dy-vec (- dy))))
              (c (vec-add mid-off (vec-mul dy-vec dy)))
              (b (vec-add a (vec-mul dx-vec x)))
              (d (vec-add c (vec-mul dx-vec x))))
         (finish a b c d)))
      ((eq? dir 'left)
       (let* ((dx-vec (vec-from-to urhc ulhc))
              (dy-vec (vec-from-to ulhc llhc))
              (mid-dir (vec-add mid (vec-mul dx-vec (+ 1 p3))))
              (mid-off (vec-add mid-dir (vec-mul dy-vec p4)))
              (b (vec-add mid-off (vec-mul dy-vec (- dy))))
              (d (vec-add mid-off (vec-mul dy-vec dy)))
              (a (vec-add b (vec-mul dx-vec x)))
              (c (vec-add d (vec-mul dx-vec x))))
         (finish a b c d)))
      ((eq? dir 'up)
       (let* ((dx-vec (vec-from-to ulhc urhc))
              (dy-vec (vec-from-to llhc ulhc))
              (mid-dir (vec-add mid (vec-mul dy-vec (+ 1 p3))))
              (mid-off (vec-add mid-dir (vec-mul dx-vec p4)))
              (c (vec-add mid-off (vec-mul dx-vec (- dx))))
              (d (vec-add mid-off (vec-mul dx-vec dx)))
              (a (vec-add c (vec-mul dy-vec y)))
              (b (vec-add d (vec-mul dy-vec y))))
         (finish a b c d)))
      ((eq? dir 'down)
       (let* ((dx-vec (vec-from-to ulhc urhc))
              (dy-vec (vec-from-to ulhc llhc))
              (mid-dir (vec-add mid (vec-mul dy-vec (+ 1 p3))))
              (mid-off (vec-add mid-dir (vec-mul dx-vec p4)))
              (a (vec-add mid-off (vec-mul dx-vec (- dx))))
              (b (vec-add mid-off (vec-mul dx-vec dx)))
              (c (vec-add a (vec-mul dy-vec y)))
              (d (vec-add b (vec-mul dy-vec y))))
         (finish a b c d)))
      ((eq? dir 'centered)
       (let* ((dx-vec (vec-from-to ulhc urhc))
              (dy-vec (vec-from-to ulhc llhc))
              (a (vec-add (vec-add mid (vec-mul dx-vec (- p3))) (vec-mul dy-vec (- p4))))
              (b (vec-add a (vec-mul dx-vec x)))
              (c (vec-add a (vec-mul dy-vec y)))
              (d (vec-add b (vec-mul dy-vec y))))
         (finish a b c d)))
      (else (error "region-relative-to: unknown direction" dir)))))

(When ((?someone wishes (?p (region ?name ?dir) (?base ?x ?y ?p3 ?p4))))
 do (let* ((ulhc (car ?base)) (urhc (cadr ?base))
           (llhc (caddr ?base)) (lrhc (cadddr ?base))
           (region (region-relative-to ?dir ulhc urhc llhc lrhc ?x ?y ?p3 ?p4)))
      (Claim ?p `(region ,?name) region)))

; TODO: below are deprecated and should be replaced/removed

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

(When ((?someone wishes (?p has-region-from-tag
  (?region ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4)))
       (?p (region page-points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let ((region (region-from-tag ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4
                                   ?ulhc ?urhc ?llhc ?lrhc)))
      (Claim ?p `(region ,?region) region)))

; asserts the region adjusted for rotation:
; this is useful when drawing as if axis-aligned, then rotating back
(When ((?someone wishes (?p has-region-from-tag-unrotated
  (?region ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4)))
       (?p (region page-points) (?ulhc ?urhc ?llhc ?lrhc))
       (?p (page rotation) ?rotation)) ; clockwise rotation
 do (let* ((region (region-from-tag ?dx1 ?dy1 ?dx2 ?dy2 ?dx3 ?dy3 ?dx4 ?dy4
                                    ?ulhc ?urhc ?llhc ?lrhc))
           (ulhc (car region)) (urhc (cadr region))
           (llhc (caddr region)) (lrhc (cadddr region))
           (aabb-pts (rotate-rect ulhc urhc llhc lrhc ?rotation)))
        (Claim ?p `(region ,?region) (cons ?rotation aabb-pts))))
