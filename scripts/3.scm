(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "datalog")

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 30 5)) (dy (/ 30 5)))
      (Wish-derived this this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish-derived this this 'has-region-from-tag-unrotated
       `(wiki ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))

(define (elem->str x)
  (if (string? x) x
    (case (car x)
      ((b) (car (last-pair x)))
      ((a) (car (last-pair x))) ; todo: assert link?
      (else ""))))

(define (p->str p)
  (string-join (map elem->str (cdr p)) ""))

; can assume axis-aligned region to draw in
(define (draw-wiki-text sxml rotation ulhc urhc llhc lrhc)
  (let* ((font 0) (scale 0.5) (thickness 1) (padding 2)
         (img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
         (msk (create-image 1280 720 0)) ; 0 is 1-channel CV8U
         (center (vec->ints (vec-add ulhc (vec-mul (vec-from-to ulhc lrhc) 0.5))))
         (cx (car center)) (cy (cdr center))
         (m (rotation-matrix-2d cx cy (- rotation) 1.0))) ; assumes counter-clockwise rotation!
    (let loop ((lst sxml) (i 0) (tx (car ulhc)))
      (if (and (< i 5) (not (null? lst)))
          (let* ((str (elem->str (car lst)))
                 (testsize (text-size str font scale thickness))
                 (width (car testsize))
                 (height (cadr testsize))
                 (baseline (caddr testsize)) ; todo: use baseline
                 (strptr (string->pointer str))
                 (ntx (+ tx padding))
                 (ty (+ (cdr ulhc) height padding)))
            (put-text img strptr tx ty font scale 255 255 255 thickness)  ; draw color to 3-channel img
            (put-text msk strptr tx ty font scale 255 255 255 thickness)  ; draw white to 1-channel mask
            (loop (cdr lst) (+ i 1) (+ tx width padding)))))
    (warp-affine img img m 1280 720) ; officially doesn't support in-place modification?
    (warp-affine msk msk m 1280 720)
    (copy-from-to img projection msk)
    (free-image img)
    (free-image msk)
    (free-image m)))

(When ((,this has-region (wiki ,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,this (page rotation) ,?rotation))
 do (let ((res (get-url (string-append urlpref topic))))
      (if res
        (let* ((sxml (parse-ssax res))
               (matched ((sxpath '(// p)) sxml))
               (first (cdar matched)))
          (draw-wiki-text first ?rotation ?ulhc ?urhc ?llhc ?lrhc))
        (Wish-derived this this 'labeled "LOADING"))))
