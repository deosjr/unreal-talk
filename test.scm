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

(define (read-page-code id)
  (call-with-input-file (format #f "scripts/~d.scm" id) (lambda (port)
    (get-string-all port)) #:encoding "utf-8"))

(define (load-page id)
  (let* ((str (read-page-code id))
         (proc (eval-string (format #f "(make-page-code ~a)" str))))
    ; todo: assert the string version as well
    (dl-assert! (get-dl) id '(page code) proc)
    (hash-set! *procs* id proc)))

(load-page 1)
(load-page 4)
(load-page 11)
(load-page 12)
