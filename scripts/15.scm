; graph plotting total production in MW
; x-axis is time, y-axis is MW
; todo: move MW specific logic out to a separate page
; and offer only support for drawing arbitratry data points

(define maxlen 100)
(define consumption-data '())
(define production-data '())
(define frequency-data '())
(define minMW 0)
(define maxMW 500)
(define minHz 4900)
(define maxHz 5100)

(define (add-data-point data x)
  (let ((newdata (if (< (length data) maxlen)
                     data
                     (reverse (cdr (reverse data))))))
    (cons x newdata)))

(When ((,?grid total-mw-consumption ,?consMW))
 do (set! consumption-data (add-data-point consumption-data ?consMW)))

(When ((,?grid total-mw-production ,?prodMW))
 do (set! production-data (add-data-point production-data ?prodMW)))

(When ((,?grid grid-frequency ,?freq))
 do (let ((freq (inexact->exact (round (* 100 ?freq)))))
      (set! frequency-data (add-data-point frequency-data freq))))

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((margin (/ 7 5)) (dx (/ 30 5)) (dy (/ 30 5)))
      (Wish-derived this this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish-derived this this 'has-region-from-tag 
       `(graph ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))

(When ((,this has-region (graph ,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((graph (list ?ulhc ?urhc ?llhc ?lrhc)))
      (fill-poly projection (bytevector->pointer (points->bytevector ?ulhc ?urhc ?lrhc ?llhc)) 4 100 100 100)
      (Wish-derived this this 'plot-data `(,consumption-data ,minMW ,maxMW ,maxlen ,graph (255 0 0)))
      (Wish-derived this this 'plot-data `(,production-data ,minMW ,maxMW ,maxlen ,graph (0 255 0)))
      (Wish-derived this this 'plot-data `(,frequency-data ,minHz ,maxHz ,maxlen ,graph (0 0 255)))))

(When ((,?someone wishes (,?someone plot-data (,?data ,?minY ,?maxY ,?stepsX (,?ulhc ,?urhc ,?llhc ,?lrhc) (,?r ,?g ,?b)))))
 do (let* ((lvec (vec-from-to ?urhc ?ulhc))
           (uvec (vec-from-to ?llhc ?ulhc)))
    (if (not (null? ?data))
      (let loop ((i 0) (lst (cdr ?data)) (last (car ?data)))
        (if (not (null? lst))
          (let* ((start (vec->ints (vec-add (vec-add ?lrhc (vec-mul lvec (/ i ?stepsX)))
                                                          (vec-mul uvec (/ last ?maxY)))))
                 (startx (car start)) (starty (cdr start))
                 (end (vec->ints (vec-add (vec-add ?lrhc (vec-mul lvec (/ (+ i 1) ?stepsX)))
                                                        (vec-mul uvec (/ (car lst) ?maxY)))))
                 (endx (car end)) (endy (cdr end)))
            (draw-line projection startx starty endx endy ?r ?g ?b 2)
            (loop (+ i 1) (cdr lst) (car lst))))))))
