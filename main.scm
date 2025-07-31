(include "opencv.scm")
(include "realtalk.scm")
(use-modules (rnrs bytevectors)
             (ice-9 format)
             (ice-9 eval-string)
             (ice-9 textual-ports))

(define font-path "/System/Library/Fonts/Menlo.ttc")
(define ft (create-freetype (string->pointer font-path)))
;(destroy-freetype ft)

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

(load-background-page 3) ; labeling
(load-background-page 6) ; whiskers
(load-background-page 9) ; highlight

; todo: load when first seeing a tag?
(load-page 1)
(load-page 2)
(load-page 4)
(load-page 7)
(load-page 8)
(load-page 10)
(load-page 11)
(load-page 12)
