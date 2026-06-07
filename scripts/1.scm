; this script was made for a 9x9cm tag in the upper lefthand corner of an A4 paper

(Wish this 'has-whiskers #t)

(define line-num 0)
(define cursor-x 0)
(define pageid -1)
(define mode 'command) ; command/insert/replace
(define code-under-edit '())
(define font-height 10) ; todo: make relative to tag size

(When ((this line-num ?line-num) ; from Remember
       (this cursor-x ?x)
       (this pageid ?id)
       (this mode ?mode)
       (this code-under-edit ?code))
 do (set! line-num ?line-num)
    (set! cursor-x ?x)
    (set! pageid ?id)
    (set! mode ?mode)
    (set! code-under-edit ?code))

(When ((key down ?k))
 do (cond ((eq? mode 'command) (command-mode ?k))
          ((eq? mode 'replace) (replace-mode ?k))
          (else (insert-mode ?k))))

(define (command-mode key)
  (case key
    ((106) (move-cursor-down)) ; j
    ((107) (move-cursor-up)) ; k
    ((104) (move-cursor-left)) ; h
    ((108) (move-cursor-right)) ; l
    ((120) (delete-under-cursor)) ; x
    ((100) (delete-current-line)) ; d
    ((105) (change-mode 'insert)) ; i
    ((114) (change-mode 'replace)) ; r  — consumes the next key in replace-mode
    ((111) (open-below)) ; o  — new line below + insert mode
    ((115) (save-page pageid (string-join code-under-edit "\n"))) ; s
  ))

(define (change-mode mode)
  (Remember this this 'mode mode))

(define (move-cursor-down)
  (if (< line-num (- (length code-under-edit) 1))
    (Remember this this 'line-num (+ line-num 1))))

(define (move-cursor-up)
  (if (> line-num 0)
    (let ((new-num (- line-num 1)))
      (Remember this this 'line-num new-num))))

(define (move-cursor-left)
  (if (> cursor-x 0)
    (let ((newx (- cursor-x 1)))
      (Remember this this 'cursor-x newx))))

(define (move-cursor-right)
  (if (< cursor-x (- (string-length (list-ref code-under-edit line-num)) 1))
    (Remember this this 'cursor-x (+ cursor-x 1))))

(define (delete-under-cursor)
  (let* ((code-len (length code-under-edit))
         (line (list-ref code-under-edit line-num))
         (line-len (string-length line)))
  (if (< cursor-x (- line-len 1))
    (let* ((left (substring/copy line 0 cursor-x))
           (right (substring/copy line (+ cursor-x 1) line-len))
           (new-line (string-append left right)))
      (list-set! code-under-edit line-num new-line)
      (Remember this this 'code-under-edit code-under-edit)))))

(define (delete-current-line)
  (if (= line-num 0)
    (set! code-under-edit (cdr code-under-edit))
    (list-cdr-set! code-under-edit (- line-num 1) (list-tail code-under-edit (+ line-num 1))))
  (Remember this this 'code-under-edit code-under-edit))

(define (insert-mode key)
  (if (= key 27) ; escape
    (change-mode 'command)
    (insert key)))

(define (insert key)
  (let* ((c (integer->char key))
         (line (list-ref code-under-edit line-num))
         (left (substring/copy line 0 cursor-x))
         (right (substring/copy line cursor-x))
         (new-line (string-append left (string c) right)))
    (list-set! code-under-edit line-num new-line)
    (move-cursor-right)
    (Remember this this 'code-under-edit code-under-edit)))

(define (replace-mode key)
  (if (= key 27) ; escape
    (change-mode 'command)
    (begin
      (replace-char key)
      (change-mode 'command))))

(define (replace-char key)
  (let* ((c (integer->char key))
         (line (list-ref code-under-edit line-num))
         (line-len (string-length line))
         (left (substring/copy line 0 cursor-x))
         (right (if (< (+ cursor-x 1) line-len)
                    (substring/copy line (+ cursor-x 1) line-len)
                    ""))
         (new-line (string-append left (string c) right)))
    (list-set! code-under-edit line-num new-line)
    (Remember this this 'code-under-edit code-under-edit)))

(define (open-below)
  (let* ((before (list-head code-under-edit (+ line-num 1)))
         (after  (list-tail code-under-edit (+ line-num 1)))
         (new-code (append before '("") after)))
    (set! code-under-edit new-code)
    (Remember this this 'code-under-edit new-code)
    (Remember this this 'line-num (+ line-num 1))
    (Remember this this 'cursor-x 0)
    (change-mode 'insert)))

(When ((this points-at ?p)
       (?p (page code) ?str))
 do (Claim this 'editing ?p)
    (if (not (= pageid ?p))
      (let ((code (string-split ?str #\newline)))
        (Remember this this 'pageid ?p)
        (Remember this this 'line-num 0)
        (Remember this this 'cursor-x 0)
        (Remember this this 'mode 'command)
        (Remember this this 'code-under-edit code))))

; inner dimensions of 9x9 tag at 1cm per pixel: 5x5cm. a4 in cm: 21 x 29.7
; tag printed with 2cm margin top and left, editor with margin below tag
(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (- (/ 4 5))) (dx (/ 17 5)) (dy (/ 25.7 5))
           (emargin (- (/ 2 5))) (edy1 (/ 9 5)) (edy2 (/ 23.7 5)) (edx (/ 15 5)))
      (Wish this 'has-region-from-tag 
       `(outline ,margin ,margin ,dx ,margin ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(editor ,emargin ,edy1 ,edx ,edy1 ,emargin ,edy2 ,edx ,edy2))))

; x and y are lower left corner in aab. rotation is left to the caller.
; caller is also assumed to draw onto a poly-fill, ie mask includes text already.
(define (draw-editor-line img str x y height r g b)
    (ft-put-text ft img (string->pointer str) x y height r g b))  ; draw color to 3-channel img

; assumes drawing from ULHC (0, 0)
(define (draw-editor-lines img dx dy char-width line-height r g b)
  (let* ((line-y (* line-height line-num))
         (cx (* char-width cursor-x))
         (ytotal dy))
    (draw-rectangle img 0 0 dx dy 0 0 255 -1)
    (draw-rectangle img 0 line-y dy (+ line-y line-height) 100 100 255 -1)
    (draw-rectangle img cx line-y (+ cx char-width) (+ line-y line-height) 150 150 255 -1)
    (let loop ((lst code-under-edit) (y 1))
      (let ((dy (* y line-height)))
        (if (and (< dy ytotal) (not (null? lst)))
          (let ((line (car lst)))
            (draw-editor-line img line 0 dy font-height 255 255 255)
            (loop (cdr lst) (+ y 1))))))))

; editor is unrotated, i.e. axis-aligned with ulhc at upper left-hand corner
(When ((this has-region (editor ?rotation ?ulhc ?urhc ?llhc ?lrhc))
       (this editing ?p))
 do (let* ((textsize (ft-text-size ft "gh" font-height)) ;gh give upper/lower bounds for line
           ;(charwidth (+ 1 (inexact->exact (round (/ (car textsize) 2))))) ; assumes mono font! also, off-by-one??
           (charwidth (inexact->exact (round (/ (car textsize) 2)))) ; assumes mono font! also, off-by-one??
           (lineheight (+ (cadr textsize) 8)) ; 8 padding pixels
           (dx (- (car ?urhc) (car ?ulhc)))
           (dy (- (cdr ?llhc) (cdr ?ulhc)))
           (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
        (draw-editor-lines img dx dy charwidth lineheight 255 255 255)
        (draw-mat-onto-region img ?rotation ?ulhc ?urhc ?llhc ?lrhc) ; draws and scales the _entire_ image into region
        (free-image img)))
