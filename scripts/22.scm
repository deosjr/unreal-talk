(Wish this 'has-whiskers #t)

(define t_iter 0)
(define prev '())
(define curr '())

(define (check-swap-buffers t)
  (if (not (equal? t t_iter))
    (begin
      (set! prev curr)
      (set! curr '())
      (set! t_iter t))))

; store claimed facts in a buffer
(When ((this points-at ?p)
       (time now ?t)
       (?someone claims (?p ?attr ?v))) do
  (check-swap-buffers ?t)
  (Wish ?someone 'highlighted 'green)
  (set! curr (cons (cons ?attr ?v) curr)))

(define (stringify claim)
  (format #f "~a: ~a" (car claim) (cdr claim)))

; do smth with claims aggregated in previous fixpoint iteration
(When ((time now ?t)
       (?p (region claimlist) (?rotation ?ulhc ?urhc ?llhc ?lrhc))) do
  (check-swap-buffers ?t)
  (let* ((strs (map stringify prev))
         (lines (sort strs string<?))
         (dx (- (car ?lrhc) (car ?ulhc)))
         (dy (- (cdr ?lrhc) (cdr ?ulhc)))
         (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
    (draw-rectangle img 0 0 dx dy 0 0 0 -1)
    (draw-text lines img (cons 0 0) (cons dx dy))
    (draw-mat-onto-region-opaque img projection ?rotation ?ulhc ?lrhc)
    (free-image img)))

(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(claimlist ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))
