(include "opencv.scm")
(include "realtalk.scm")
(use-modules (rnrs bytevectors)
             (ice-9 format)
             (ice-9 eval-string)
             (ice-9 textual-ports))

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

(load-background-page 6) ; whiskers

; todo: load when first seeing a tag?
(load-page 1)
(load-page 4)
(load-page 11)
(load-page 12)
