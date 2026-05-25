(define card-name "Black Lotus")
(define card-set #f)  ; e.g. "LEA", or #f to let Scryfall pick

(define (url-encode str)
  (let loop ((i 0) (acc '()))
    (if (= i (string-length str))
        (apply string-append (reverse acc))
        (let ((c (string-ref str i)))
          (loop (+ i 1)
                (cons (cond
                        ((or (char-alphabetic? c) (char-numeric? c)
                             (char=? c #\-) (char=? c #\_)
                             (char=? c #\.) (char=? c #\~))
                         (string c))
                        ((char=? c #\space) "%20")
                        (else (format #f "%~2,'0X" (char->integer c))))
                      acc))))))

(define (api-url name set)
  (let ((base (string-append "https://api.scryfall.com/cards/named?exact="
                             (url-encode name))))
    (if set (string-append base "&set=" (url-encode set)) base)))

(define (find-substring needle haystack)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (let loop ((i 0))
      (cond ((> (+ i nlen) hlen) #f)
            ((string=? needle (substring haystack i (+ i nlen))) i)
            (else (loop (+ i 1)))))))

(define (index-char ch s start)
  (let loop ((i start))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

;; Scryfall's JSON has "image_uris":{"small":...,"normal":...,"large":...}.
;; We grab "large". Works for single-faced cards (the common case);
;; double-faced cards have image_uris nested per face — left as a TODO.
(define (extract-large-image json-str)
  (let* ((key "\"large\":\"")
         (klen (string-length key))
         (start (find-substring key json-str)))
    (and start
         (let* ((url-start (+ start klen))
                (url-end (index-char #\" json-str url-start)))
           (and url-end (substring json-str url-start url-end))))))

(define (decode-bytes bytes)
  (decode-image (bytevector->pointer bytes) (bytevector-length bytes)))

; Region geometry: multipliers are on the tag's local dx/dy basis
; vectors (see scripts/9004.scm).
(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (Wish this 'has-region-from-tag-unrotated
     `(image 2 0 6 0 2 6 6 6)))

(define (draw-image src rotation ulhc urhc llhc lrhc)
  (let* ((ulhcx (car ulhc)) (ulhcy (cdr ulhc))
         (dx (- (car urhc) ulhcx))
         (dy (- (cdr llhc) ulhcy))
         (center (vec->ints (vec-add ulhc (vec-mul (vec-from-to ulhc lrhc) 0.5))))
         (cx (car center)) (cy (cdr center))
         (minv (rotation-matrix-2d cx cy (- rotation) 1.0))
         (img (create-image 1280 720 16))   ; 3-channel BGR
         (mask (create-image 1280 720 0))   ; 1-channel
         (dest (region img ulhcx ulhcy dx dy)))
    (resize src dest dx dy 0 0 3)           ; INTER_AREA
    (draw-rectangle mask ulhcx ulhcy (+ ulhcx dx) (+ ulhcy dy) 255 255 255 -1)
    (warp-affine img img minv 1280 720)
    (warp-affine mask mask minv 1280 720)
    (copy-from-to img projection mask)
    (free-image img)
    (free-image mask)
    (free-image dest)
    (free-image minv)))

(When ((this has-region (image ?rotation ?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((image-url (get-url-with-proc (api-url card-name card-set)
                                         extract-large-image))
           (src (and image-url
                     (get-url-bytes-with-proc image-url decode-bytes))))
      (if src
        (draw-image src ?rotation ?ulhc ?urhc ?llhc ?lrhc)
        (Wish this 'subtitled (string-append "LOADING " card-name)))))
