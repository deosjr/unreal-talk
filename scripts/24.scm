; evil mode - VIM for emacs
; communicates to all editors; eventually should be on a swappable keyboard tag

(define bg '(0 0 255))
(define line '(100 100 255))
(define cursor '(150 150 255))
(define text '(255 255 255))

(When ((?editor editor #t))
 do (Wish ?editor 'has-background-color bg)
    (Wish ?editor 'has-line-color line)
    (Wish ?editor 'has-cursor-color cursor)
    (Wish ?editor 'has-text-color text))

(When ((key down ?k))
 do (let ((c (integer->char ?k)))
     (Wish this 'subtitled (string c))))

; NOTE: these Wishes currently cannot move to separate functions
; as that would break macro-expansion into Wish-derived :/
; derived-wish! is available, but not intended to be used directly
(When ((?editor mode command)
       (?editor editing ?p)
       (key down ?key))
 do (case ?key
       ((48)  (Wish ?editor 'edits '((start-of-line))))   ; 0
       ((36)  (Wish ?editor 'edits '((end-of-line))))     ; $
       ((104) (Wish ?editor 'edits '((forward-char -1)))) ; h
       ((106) (Wish ?editor 'edits '((forward-line 1))))  ; j
       ((107) (Wish ?editor 'edits '((forward-line -1)))) ; k
       ((108) (Wish ?editor 'edits '((forward-char 1))))  ; l
       ((120) (Wish ?editor 'edits '((delete-char 1))))   ; x
       ((105) (Wish ?editor 'edits '((change-mode insert))))  ; i
       ((114) (Wish ?editor 'edits '((change-mode replace)))) ; r
       ((111) (Wish ?editor 'edits '((end-of-line) (new-line) (change-mode insert))))   ; o
       ((115) (Wish ?editor 'edits '((save)))) ; s 
     ))

(When ((?editor mode replace)
       (?editor editing ?p)
       (key down ?key))
 do (if (not (= ?key 27)) ; escape
      (let* ((c (integer->char ?key))
             (s (string c)))
        (Wish ?editor 'edits `((delete-char 1) (insert ,s)))))
      (Wish ?editor 'edits '((change-mode command))))

(define (insert-char editor key)
  (let* ((c (integer->char key))
         (s (string c)))
    (Wish editor 'edits `((insert ,s)))))

(When ((?editor mode insert)
       (?editor editing ?p)
       (key down ?key))
 do (case ?key
      ((127) (Wish ?editor 'edits '((forward-char -1) (delete-char 1)))) ; backspace
      ((13)  (Wish ?editor 'edits '((new-line)))) ; newline
      ((27)  (Wish ?editor 'edits '((change-mode command)))) ; escape
      (else ; insert char
        (let* ((c (integer->char ?key))
               (s (string c)))
          (Wish ?editor 'edits `((insert ,s)))))))
