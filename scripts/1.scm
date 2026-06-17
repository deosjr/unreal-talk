; this script was made for a 9x9cm tag in the upper lefthand corner of an A4 paper
; this is the low-level editor, owning buffer and cursor
; editing behaviour is editable, like in emacs, through RealTalk

(Claim this 'editor #t)
(Wish this 'has-whiskers #t)

(define line-num 0)
(define cursor-x 0)
(define scroll-offset 0)
(define pageid -1)
(define mode 'command) ; command/insert/replace
(define buffer '())

(When ((this line-num ?line-num) ; from Remember
       (this cursor-x ?x)
       (this scroll-offset ?so)
       (this pageid ?id)
       (this mode ?mode)
       (this code-under-edit ?code))
 do (set! line-num ?line-num)
    (set! cursor-x ?x)
    (set! scroll-offset ?so)
    (set! pageid ?id)
    (set! mode ?mode)
    (set! buffer ?code))

(define (remember)
  (Remember this this 'line-num line-num)
  (Remember this this 'cursor-x cursor-x)
  (Remember this this 'scroll-offset scroll-offset)
  (Remember this this 'mode mode)
  (Remember this this 'code-under-edit buffer))

(When ((?someone wishes (this edits ?list)))
 do (for-each edit ?list)
    (remember))

(define (edit op)
  (case (car op)
    ((change-mode) (change-mode (cadr op)))
    ((start-of-line) (start-of-line))
    ((end-of-line) (end-of-line))
    ((forward-char) (forward-char (cadr op)))
    ((forward-line) (forward-line (cadr op)))
    ((delete-char) (delete-char (cadr op)))
    ((insert) (insert (cadr op)))
    ((new-line) (new-line))
    ((save) (save))
  ))

(define (change-mode m)
  (set! mode m))

(define (forward-char n)
  (let ((x (+ cursor-x n))
        ; NOTE: we can sit at index len, meaning one past end of line!
        (len (string-length (list-ref buffer line-num))))
    (if (< x 0) (set! cursor-x 0)
      (if (> x len) (set! cursor-x len)
        (set! cursor-x x)))))

(define (forward-line n)
  (let ((x (+ line-num n))
        (len (- (length buffer) 1)))
    (if (< x 0) (set! line-num 0)
      (if (> x len) (set! line-num len)
        (set! line-num x)))))

(define (start-of-line)
  (set! cursor-x 0))

(define (end-of-line)
  (set! cursor-x (string-length (list-ref buffer line-num))))

(define (scroll visible)
  (cond ((< line-num scroll-offset)
         (set! scroll-offset line-num))
        ((>= line-num (+ scroll-offset visible))
         (set! scroll-offset (+ (- line-num visible) 1)))))

; TODO: n other than 1
(define (delete-char n)
  (let* ((code-len (length buffer))
         (line (list-ref buffer line-num))
         (line-len (string-length line)))
  (if (< cursor-x (- line-len 1))
    (let* ((left (substring/copy line 0 cursor-x))
           (right (substring/copy line (+ cursor-x 1) line-len))
           (new-line (string-append left right)))
      (list-set! buffer line-num new-line)))))

(define (insert s)
  (let* ((line (list-ref buffer line-num))
         (left (substring/copy line 0 cursor-x))
         (right (substring/copy line cursor-x))
         (new-line (string-append left s right)))
    (list-set! buffer line-num new-line)
    (forward-char 1)))

(define (new-line)
  (let* ((line (list-ref buffer line-num))
         (left (substring/copy line 0 cursor-x))
         (right (substring/copy line cursor-x))
         (before (list-head buffer line-num))
         (after (list-tail buffer (+ line-num 1))))
    (set! buffer (append before (list left right) after))
    (forward-line 1)
    (start-of-line)))

(define (save)
  (save-page pageid (string-join buffer "\n")))

(When ((this points-at ?p)
       (?p (page code) ?str))
 do (Claim this 'editing ?p)
    (if (not (= pageid ?p))
      (let ((code (string-split ?str #\newline)))
        (Remember this this 'pageid ?p)
        (Remember this this 'line-num 0)
        (Remember this this 'cursor-x 0)
        (Remember this this 'scroll-offset 0)
        (Remember this this 'mode 'command)
        (Remember this this 'code-under-edit code))))

; NOTE: like key bindings, how to draw the buffer can be handed off to a different tag
; For now, we make assumptions and draw a background, a highlighted line, a cursor,
; and then the text in the buffer (which has a single color only).

(define background-color '(0 0 255))
(define line-color '(100 100 255))
(define cursor-color '(150 150 255))
(define text-color '(255 255 255))

; remember the above as default
(When ((time now ?t)) do
  (Remember this this 'background-color background-color)
  (Remember this this 'line-color line-color)
  (Remember this this 'cursor-color cursor-color)
  (Remember this this 'text-color text-color))

; restore default every iteration
(When ((this background-color ?bg)
       (this line-color ?l)
       (this cursor-color ?c)
       (this text-color ?t))
 do (set! background-color ?bg)
    (set! line-color ?l)
    (set! cursor-color ?c)
    (set! text-color ?t))

; then let someone else overwrite it using a Wish (which fires later)
(When ((?someone wishes (this has-background-color ?c)))
 do (set! background-color ?c))

(When ((?someone wishes (this has-line-color ?c)))
 do (set! line-color ?c))

(When ((?someone wishes (this has-cursor-color ?c)))
 do (set! cursor-color ?c))

(When ((?someone wishes (this has-text-color ?c)))
 do (set! text-color ?c))

(define (draw-editor-background img dx dy char-width line-height)
  (let* ((line-y (* line-height (- line-num scroll-offset)))
         (cx (* char-width cursor-x))
         (bgR (car background-color)) (bgG (cadr background-color)) (bgB (caddr background-color))
         (lR (car line-color)) (lG (cadr line-color)) (lB (caddr line-color))
         (cR (car cursor-color)) (cG (cadr cursor-color)) (cB (caddr cursor-color)))
    (draw-rectangle img 0 0 dx dy bgR bgG bgB -1) ; background
    (draw-rectangle img 0 line-y dy (+ line-y line-height) lR lG lB -1) ; current line
    (draw-rectangle img cx line-y (+ cx char-width) (+ line-y line-height) cR cG cB -1))) ; cursor

; editor is unrotated, i.e. axis-aligned with ulhc at upper left-hand corner
(When ((this has-region (editor ?rotation ?ulhc ?urhc ?llhc ?lrhc))
       (this editing ?p))
 do (let* ((dx (- (car ?lrhc) (car ?ulhc)))
           (dy (- (cdr ?lrhc) (cdr ?ulhc)))
           (visible-lines (max 1 (quotient dy line-height)))
           (buffer-offset (list-tail buffer scroll-offset))
           (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
        (scroll visible-lines) ; todo: lagging one tick behind?
        (draw-editor-background img dx dy char-width line-height)
        (draw-text buffer-offset img (cons 0 0) (cons dx dy) #:color text-color)
        (draw-mat-onto-region-opaque img projection ?rotation ?ulhc ?lrhc) ; draws and scales the _entire_ image into region
        (free-image img)))
