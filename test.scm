(include "opencv.scm")
(include "realtalk.scm")
(use-modules (rnrs bytevectors))

; this img is a pointer to a cv::Mat that is sent by Golang
(define img #f)
(define (init-image ptr)
  (set! img ptr))

(define *pages-in-scene* (make-hash-table))

(define (pages-found pages)
  (fill-image img 0 0 0) ;; fill black
  (for-each (lambda (page)
    (let ((id (car page))
          (points (cadr page))
          (rotation (caddr page)))
      (update-page-geometry id points rotation))) pages)
  (dl-fixpoint! (get-dl)))

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

(define (draw-on-page ulhc urhc llhc lrhc b g r)
  (fill-poly img (bytevector->pointer (points->bytevector ulhc urhc llhc lrhc)) 4 b g r))

(define (draw-line from to width b g r)
  ; hardcoded and hacked for now
  (let* ((fromx (inexact->exact (round (car from)))) (fromy (inexact->exact (round (cdr from))))
         (tox (inexact->exact (round (car to)))) (toy (inexact->exact (round (cdr to))))
         (pts (make-bytevector (* 8 4)))
         (_ (begin
           (bytevector-s32-native-set! pts 0 (- fromx 1))
           (bytevector-s32-native-set! pts 4 fromy)
           (bytevector-s32-native-set! pts 8 (- tox 1))
           (bytevector-s32-native-set! pts 12 toy)
           (bytevector-s32-native-set! pts 16 (+ tox 1))
           (bytevector-s32-native-set! pts 20 toy)
           (bytevector-s32-native-set! pts 24 (+ fromx 1))
           (bytevector-s32-native-set! pts 28 fromy))))
    (fill-poly img (bytevector->pointer pts) 4 b g r)))

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

(define page4proc (make-page-code
  (Wish this 'has-whiskers #t)
  (When ((points-at ,this ,?p)
         ((page points) ,?p (,?ulhc ,?urhc ,?llhc ,?lrhc)))
   do (draw-on-page ?ulhc ?urhc ?lrhc ?llhc 0 255 0))))
(page4proc 4)

; whisker length == page height
(define page12proc (make-page-code
  (define (claim-has-whiskers p)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p has-whiskers #t)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p has-whiskers #t) #t)
    (Claim p 'has-whiskers #t))
  (define (claim-pointer-at p point)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p pointer-at ,point)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p pointer-at ,point) #t)
    (Claim p 'pointer-at point))
  (define (claim-point-at p q)
    (hash-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hash-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))

  (define pi 3.1415926536)

  (When ((wishes ,?p (,?p has-whiskers ,#t))) do
    (claim-has-whiskers ?p))

  ; page rotates around midpoint: from there to whisker end, add halfh + whisker length
  ; all of this logic becomes easier here if we do some of the calculations in modules/realtalk
  (When ((has-whiskers ,?p #t)
         ((page points) ,?p (,?ulhc ,?urhc ,?llhc ,?lrhc)))
   do (let* ((upvec (vec-from-to ?ulhc ?urhc))
             (mid (vec-add ?ulhc (vec-mul upvec 0.5)))
             (end (vec-add mid (vec-mul (vec-from-to ?lrhc ?urhc) 2)))
             (endx (inexact->exact (round (car end)))) (endy (inexact->exact (round (cdr end)))))
         (draw-line mid end 2 0 255 0)
         (claim-pointer-at ?p (cons endx endy))))

  ; NOTE: this fires for every page, since we can't calculate in the db atm!
  ; todo: rotated page now checks bounding box, not actual div dimensions
  (When ((pointer-at ,?p (,?px . ,?py))
         ((page points) ,?q (,?ulhc ,?urhc ,?llhc ,?lrhc)))
    do (let* ((pts (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))
              (ptr (bytevector->pointer pts))
              (test (point-polygon-test ptr 4 ?px ?py)))
      (if (> test 0) (claim-point-at ?p ?q))))
))
(page12proc 12)
