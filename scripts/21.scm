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
(When ((this (region page-points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (Wish this 'has-region-from-tag-unrotated
     `(image 2 0 6 0 2 6 6 6)))

(When ((this (region image) (?rotation ?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((image-url (get-url-with-proc (api-url card-name card-set)
                                         extract-large-image))
           (src (and image-url
                     (get-url-bytes-with-proc image-url decode-bytes))))
      (if src
        (draw-mat-onto-region-opaque src projection ?rotation ?ulhc ?lrhc)
        (Wish this 'subtitled (string-append "LOADING " card-name)))))
