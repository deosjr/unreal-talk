; graph plotting total production in MW
; x-axis is time, y-axis is MW

(define maxlen 100)
(define mw-data '())
(define minMW 0)
(define maxMW 1000)

(define (add-data-point data x)
  (let ((newdata (if (< (length data) maxlen)
                     data
                     (reverse (cdr (reverse data))))))
    (set! mw-data (cons x newdata))))

(When ((,?grid total-mw-production ,?prodMW))
 do (add-data-point mw-data ?prodMW))

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((margin (/ 7 5)) (dx (/ 30 5)) (dy (/ 30 5)))
      (Wish-derived this this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish-derived this this 'has-region-from-tag 
       `(graph ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))

(When ((,this has-region (graph ,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((lvec (vec-from-to ?urhc ?ulhc))
           (uvec (vec-from-to ?llhc ?ulhc)))
    (if (not (null? mw-data))
      (let loop ((i 0) (lst (cdr mw-data)) (last (car mw-data)))
        (if (not (null? lst))
          (let* ((start (vec->ints (vec-add (vec-add ?lrhc (vec-mul lvec (/ i maxlen)))
                                                          (vec-mul uvec (/ last maxMW)))))
                 (startx (car start)) (starty (cdr start))
                 (end (vec->ints (vec-add (vec-add ?lrhc (vec-mul lvec (/ (+ i 1) maxlen)))
                                                        (vec-mul uvec (/ (car lst) maxMW)))))
                 (endx (car end)) (endy (cdr end)))
            (draw-line projection startx starty endx endy 0 255 0 2)
            (loop (+ i 1) (cdr lst) (car lst))))))))
