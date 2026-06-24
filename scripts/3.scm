(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "owl")

(Claim this '(wiki topic) topic)

(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(wiki ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))

; SXML elements look like (tag (@ attrs...) children...). For inline
; styling tags we want their text content. last-pair returns the last
; child cell; recurse on its car so that nested wrappers like
; <a href><i>Canis lupus</i></a> resolve down to the leaf string
; instead of leaking a list to string->pointer.
; todo: join all children instead of just the last one
(define (elem->str x)
  (cond
    ((string? x) x)
    ((pair? x)
     (case (car x)
       ((b a i) (elem->str (car (last-pair x))))
       (else "")))
    (else "")))

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

(define (filter-first-paragraph matched)
  (if (null? matched) '()
    (cdar (filter (lambda (m) 
      (if (pair? (cdr m))
        (if (pair? (cadr m))
          (= (length (cdadr m)) 1) #f) #f))
     matched))))

(define (get-first-paragraph res)
  (let* ((sxml (parse-ssax res))
         (matched ((sxpath '(// (section (@ data-mw-section-id (equal? "0"))) // p )) sxml))
         (first (filter-first-paragraph matched)))
first))

(define (find-href a)
  (substring (cadr (assq 'href (cdr (assq '@ (cdr a))))) 2))

; can assume axis-aligned region to draw in
(define (draw-wiki-text sxml rotation ulhc urhc llhc lrhc claim-func)
  (let* ((font 0) (scale 0.7) (thickness 1) (padding 2)
         (img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
         (msk (create-image 1280 720 0)) ; 0 is 1-channel CV8U
         (center (vec->ints (vec-add ulhc (vec-mul (vec-from-to ulhc lrhc) 0.5))))
         (cx (car center)) (cy (cdr center))
         (m (rotation-matrix-2d cx cy (- rotation) 1.0)) ; assumes counter-clockwise rotation!
         (dx (car (vec->ints (vec-from-to ulhc urhc))))
         (dy (cdr (vec->ints (vec-from-to ulhc llhc))))
         (tx (+ (car ulhc) padding)) (ty (+ (cdr ulhc) padding))
         (max-y (+ (cdr ulhc) dy (- padding))))
    (let loop ((lst (break-up sxml '())) (i 0) (x tx) (y ty) (w 0))
      (if (and (< i 100) (< y max-y) (not (null? lst)))
          (let* ((elem (car lst))
                 (str (elem->str elem))
                 ; todo: if bold (b), thickness + 1
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
            (if (and (pair? elem) (eq? (car elem) 'a)) ; hyperlink
              (let ((ulhc (cons nx nny))
                    (urhc (cons (+ nx width) nny))
                    (llhc (cons nx ny))
                    (lrhc (cons (+ nx width) ny)))
                (fill-poly-img img ulhc urhc lrhc llhc 200 100 100)
		; this claim needs unrotated points!
                (let ((unrotated (rotate-points-around cx cy (- rotation) ulhc urhc llhc lrhc)))
                  (claim-func (cons (find-href elem) unrotated)))))
            (put-text img strptr nx ny font scale 255 255 255 thickness)  ; draw color to 3-channel img
            (fill-poly-img msk (cons nx nny) (cons (+ nx width) nny) (cons (+ nx width) ny) (cons nx ny) 255 255 255)
            (loop (cdr lst) (+ i 1) (+ nx width padding) (- ny height) nw))))
    (warp-affine img img m 1280 720) ; officially doesn't support in-place modification?
    (warp-affine msk msk m 1280 720)
    (copy-from-to img projection msk)
    (free-image img)
    (free-image msk)
    (free-image m)))

(When ((?p (region wiki) (?rotation ?ulhc ?urhc ?llhc ?lrhc))
       (?p (wiki topic) ?topic))
 do (let* ((url (string-append urlpref ?topic))
           (res (get-url-with-proc url get-first-paragraph))
           (claim-func (lambda (data) (Claim this 'wiki-link data))))
      (if res
        (draw-wiki-text res ?rotation ?ulhc ?urhc ?llhc ?lrhc claim-func)
        (Wish this 'subtitled "LOADING"))))

(When ((?p pointer-at (?px . ?py))
       (?q wiki-link (?topic ?ulhc ?urhc ?llhc ?lrhc)))
  do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
            (ptr (bytevector->pointer pts))
            (test (point-polygon-test ptr 4 ?px ?py)))
    (if (> test 0) (Claim ?p 'points-at-wiki-link ?topic))))

(When ((?p (region wiki) (?rotation ?ulhc ?urhc ?llhc ?lrhc))
       (?p points-at-wiki-link ?topic))
 do (let* ((url (string-append urlpref ?topic))
           (res (get-url-with-proc url get-first-paragraph))
           (claim-func (lambda (data) (Claim ?p 'wiki-link data))))
      (if res
        (draw-wiki-text res ?rotation ?ulhc ?urhc ?llhc ?lrhc claim-func)
        (Wish ?p 'subtitled "LOADING"))))
