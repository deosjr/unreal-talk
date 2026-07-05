; evil mode - VIM for emacs
; communicates to all editors; eventually should be on a swappable keyboard tag

(define bg '(0 0 0))
(define line '(100 100 100))
(define cursor '(150 150 150))
(define text '(0 255 0))

(When ((?editor editor #t))
 do (Wish ?editor 'has-background-color bg)
    (Wish ?editor 'has-line-color line)
    (Wish ?editor 'has-cursor-color cursor)
    (Wish ?editor 'has-text-color text))

; TODO: use parameters?
; TODO: should live on a separate tag that makes claims about editor visuals
; but right now this lets us edit the editor dimensions from the editor
; inner dimensions of 9x9 tag at 1cm per pixel: 5x5cm. a4 in cm: 21 x 29.7
; tag printed with 2cm margin top and left, editor with margin below tag
(When ((?editor editor #t)
       (?editor (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (- (/ 4 5))) (dx (/ 17 5)) (dy (/ 25.7 5))
           (emargin (- (/ 2 5))) (edy1 (/ 9 5)) (edy2 (/ 50 5)) (edx (/ 40 5)))
      (Wish ?editor 'has-region-from-tag 
       `(outline ,margin ,margin ,dx ,margin ,margin ,dy ,dx ,dy))
      (Wish ?editor 'has-region-from-tag-unrotated
       `(editor ,emargin ,edy1 ,edx ,edy1 ,emargin ,edy2 ,edx ,edy2))))

(When ((key down ?k))
 do (let ((c (integer->char ?k)))
     (Wish this 'subtitled (string c))))

(define (get-edits mode key)
  (case mode
    ((command) (edits-command key))
    ((replace) (edits-replace key))
    ((insert)  (edits-insert key))
    (else '())))

(define (edits-command key)
  (case key
       ((48)  '((start-of-line)))   ; 0
       ((36)  '((end-of-line)))     ; $
       ((100) '((delete-line)))     ; d
       ((104) '((forward-char -1))) ; h
       ((106) '((forward-line 1)))  ; j
       ((107) '((forward-line -1))) ; k
       ((108) '((forward-char 1)))  ; l
       ((120) '((delete-char 1)))   ; x
       ((105) '((change-mode insert)))  ; i
       ((114) '((change-mode replace))) ; r
       ((111) '((end-of-line) (new-line) (change-mode insert)))   ; o
       ((115) '((save))) ; s 
       (else '())))

(define (insert-char key)
  (let* ((c (integer->char key))
         (s (string c)))
    `(insert ,s)))

(define (edits-replace key)
  (if (= key 27) ; escape
      '((change-mode command))
      `((delete-char 1) ,(insert-char key) (change-mode command))))

(define (edits-insert key)
  (case key
      ; todo: don't delete-char if we start on cursor-x = 0
      ; this may require a delete/delete-backwards split in editor?
      ((127) '((forward-char -1) (delete-char 1))) ; backspace
      ((13)  '((new-line))) ; newline
      ((27)  '((change-mode command))) ; escape
      (else `(,(insert-char key))))) ; insert

; find out which mode we land on after a series of operations
(define (mode-after ops mode)
  (fold (lambda (op m)
    (if (eq? (car op) 'change-mode) (cadr op) m))
   mode ops))

; todo: since we now fold over multiple key presses
; we can remove all but the last change-mode if we like
(When ((?editor mode ?mode)
       (?editor editing ?p)
       (key sequence ?keys))
 do (let loop ((keys ?keys) (mode ?mode) (acc '()))
      (if (null? keys)
        (if (pair? acc) (Wish ?editor 'edits acc))
        (let ((ops (get-edits mode (car keys))))
          (loop (cdr keys) (mode-after ops mode) (append acc ops))))))
