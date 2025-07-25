(include "opencv.scm")
(include "realtalk.scm")
(use-modules (rnrs bytevectors)
             (ice-9 format)
             (ice-9 eval-string)
             (ice-9 textual-ports))

(define (text-size str font scale thickness)
  (let ((buf (bytevector->pointer (make-bytevector (* 3 (sizeof int)))))
         (cstr (string->pointer str)))
    (get-text-size cstr font scale thickness buf)
    (parse-c-struct buf (list int int int))))

(define (points->bytevector a b c d)
  (let* ((pts (make-bytevector (* 8 4)))
         (_ (begin
           (bytevector-s32-native-set! pts 0 (car a))
           (bytevector-s32-native-set! pts 4 (cdr a))
           (bytevector-s32-native-set! pts 8 (car b))
           (bytevector-s32-native-set! pts 12 (cdr b))
           (bytevector-s32-native-set! pts 16 (car c))
           (bytevector-s32-native-set! pts 20 (cdr c))
           (bytevector-s32-native-set! pts 24 (car d))
           (bytevector-s32-native-set! pts 28 (cdr d))))) pts))

(define (draw-on-page ulhc urhc llhc lrhc r g b)
  (fill-poly img (bytevector->pointer (points->bytevector ulhc urhc llhc lrhc)) 4 r g b))

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
    (dl-assert! (get-dl) id '(page code) proc)
    (hash-set! *procs* id proc)))

(load-page 4)
(load-page 12)
