(include "opencv.scm")
(include "realtalk.scm")
(include "webclient.scm")
(use-modules (rnrs bytevectors)
             (ice-9 format)
             (ice-9 eval-string)
             (ice-9 textual-ports))

(define font-path "/System/Library/Fonts/Menlo.ttc")
(define ft (create-freetype (string->pointer font-path)))
;(destroy-freetype ft)

(define font-height 10)
(define textsize (ft-text-size ft "gh" font-height))
(define line-height (+ (cadr textsize) 8)) ; 8 padding pixels

(define (vec-add p q)
  (let ((px (car p)) (py (cdr p))
        (qx (car q)) (qy (cdr q)))
    (cons (+ px qx) (+ py qy))))

(define (vec-sub p q)
  (let ((px (car p)) (py (cdr p))
        (qx (car q)) (qy (cdr q)))
    (cons (- px qx) (- py qy))))

(define (vec-mul p scalar)
  (let ((px (car p)) (py (cdr p)))
    (cons (* px scalar) (* py scalar))))

(define (vec-from-to p q) (vec-sub q p))

(define (vec-length p)
  (let* ((px (car p)) (py (cdr p))
         (px2 (* px px)) (py2 (* py py)))
    (sqrt (+ px2 py2))))

(load-background-page 9001)  ; labeling
(load-background-page 9002)  ; whiskers
(load-background-page 9003)  ; highlight
(load-background-page 9004)  ; regions
(load-background-page 9005)  ; error display

; User-tag scripts (scripts/<id>.scm) are loaded on demand the first
; time the tag is seen on the table — see ensure-loaded! in realtalk.scm.
; A missing or unreadable file surfaces as a 'has-error fact (rendered as
; a subtitle by 9005). One load attempt per tag per session.
