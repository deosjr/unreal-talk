(Wish this 'has-whiskers #t)

(define t_iter 0)
(define prev '())
(define curr '())
(define font-height 10)

(define (sort-claims buffer)
  (sort buffer (lambda (a b) (string<? (stringify a) (stringify b)))))

(define (stringify p)
  (format #f "~a ~a" (car p) (cdr p)))

(define (draw-claims claims rotation ulhc urhc llhc lrhc)
  (let* ((sorted (sort-claims claims))
         (textsize (ft-text-size ft "gh" font-height))
         (lineheight (+ (cadr textsize) 8)) ; 8 padding pixels
         (dx (- (car urhc) (car ulhc)))
         (dy (- (cdr llhc) (cdr ulhc)))
         (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
    (draw-rectangle img 0 0 dx dy 0 0 0 -1)
    (let loop ((lst sorted) (y lineheight))
      (if (and (pair? lst) (< y dy))
        (let ((line (format #f "~a: ~a" (caar lst) (cdar lst))))
          (ft-put-text ft img (string->pointer line) 0 y font-height 255 255 255)
          (loop (cdr lst) (+ y lineheight)))))
    (draw-mat-onto-region-opaque img projection rotation ulhc lrhc)
    (free-image img)))

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

; do smth with claims aggregated in previous fixpoint iteration
(When ((time now ?t)
       (?p has-region (claimlist ?rotation ?ulhc ?urhc ?llhc ?lrhc))) do
  (check-swap-buffers ?t)
  (draw-claims prev ?rotation ?ulhc ?urhc ?llhc ?lrhc)) ; do smth with the aggregate in buffer

(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(claimlist ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))
