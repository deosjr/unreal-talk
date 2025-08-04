(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "datalog")

(Claim this '(wiki topic) topic)

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
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

(define (break-up sxml acc)
  (if (null? sxml)
    (reverse acc)
    (let ((head (car sxml))
          (tail (cdr sxml)))
      (if (pair? head)
        (break-up tail (cons head acc))
        (break-up tail (append (reverse (string-split head #\space)) acc))))))

; can assume axis-aligned region to draw in
(define (draw-wiki-text sxml rotation ulhc urhc llhc lrhc)
  (let* ((font 0) (scale 0.7) (thickness 1) (padding 2)
         (img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
         (msk (create-image 1280 720 0)) ; 0 is 1-channel CV8U
         (center (vec->ints (vec-add ulhc (vec-mul (vec-from-to ulhc lrhc) 0.5))))
         (cx (car center)) (cy (cdr center))
         (m (rotation-matrix-2d cx cy (- rotation) 1.0)) ; assumes counter-clockwise rotation!
         (dx (car (vec->ints (vec-from-to ulhc urhc))))
         (tx (+ (car ulhc) padding)) (ty (+ (cdr ulhc) padding)))
    (let loop ((lst (break-up sxml '())) (i 0) (x tx) (y ty) (w 0))
      (if (and (< i 100) (not (null? lst)))
          (let* ((str (elem->str (car lst)))
                 (testsize (text-size str font scale thickness))
                 (width (car testsize))
                 (height (cadr testsize))
                 (baseline (caddr testsize)) ; todo: use baseline
                 (strptr (string->pointer str))
                 (line-overflow (< dx (+ w width)))
                 (nw (if line-overflow (+ width padding) (+ w width padding)))
                 (nx (if line-overflow tx x))
                 (ny (if line-overflow (+ y height height padding) (+ y height)))
                 (nny (if line-overflow (+ y height) y)))
            (if (and (pair? (car lst)) (eq? (caar lst) 'a)) ; hyperlink
              (let ((ulhc (cons nx nny))
                    (urhc (cons (+ nx width) nny))
                    (llhc (cons nx ny))
                    (lrhc (cons (+ nx width) ny)))
                (fill-poly-img img ulhc urhc lrhc llhc 200 100 100)
                (Claim-derived this this 'wiki-link (list str ulhc urhc llhc lrhc))))
            (put-text img strptr nx ny font scale 255 255 255 thickness)  ; draw color to 3-channel img
            (fill-poly-img msk (cons nx nny) (cons (+ nx width) nny) (cons (+ nx width) ny) (cons nx ny) 255 255 255)
            (loop (cdr lst) (+ i 1) (+ nx width padding) (- ny height) nw))))
    (warp-affine img img m 1280 720) ; officially doesn't support in-place modification?
    (warp-affine msk msk m 1280 720)
    (copy-from-to img projection msk)
    (free-image img)
    (free-image msk)
    (free-image m)))

(define (get-first-paragraph res)
  (let* ((sxml (parse-ssax res))
         (matched ((sxpath '(// p)) sxml))
         (first (if (null? matched) '() (cdar matched)))) first))

(When ((,?p has-region (wiki ,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,?p (wiki topic) ,?topic)
       (,?p (page rotation) ,?rotation))
 do (let ((res (get-url (string-append urlpref topic))))
      (if res
        (draw-wiki-text (get-first-paragraph res) ?rotation ?ulhc ?urhc ?llhc ?lrhc)
        (Wish-derived this this 'labeled "LOADING"))))

(When ((,?p pointer-at (,?px . ,?py))
       (,?q wiki-link (,?topic ,?ulhc ,?urhc ,?llhc ,?lrhc)))
  do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
            (ptr (bytevector->pointer pts))
            (test (point-polygon-test ptr 4 ?px ?py)))
    (if (> test 0) (Claim-derived this ?p 'points-at-wiki-link ?topic))))

(When ((,?p has-region (wiki ,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,?p points-at-wiki-link ,?topic)
       (,?p (page rotation) ,?rotation))
 do (let ((res (get-url (string-append urlpref ?topic))))
      (if res
        (draw-wiki-text (get-first-paragraph res) ?rotation ?ulhc ?urhc ?llhc ?lrhc)
        (Wish-derived this ?p 'labeled "LOADING"))))
