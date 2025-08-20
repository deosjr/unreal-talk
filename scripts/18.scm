; pong game

(define field-width 101)
(define field-height 61)
(define ball-pos (cons 50 30))
(define ball-vel (cons 0 0))
(define left-paddle-y 27)
(define right-paddle-y 27)
(define left-paddle-v 0)
(define right-paddle-v 0)

(define left-score 0)
(define right-score 0)

; play area is 51x31 tag pixels in size, underneath the tag
; basis vectors are x and y of size 1 tag pixel
; the ball is one tag pixel in diameter
; paddles are 5 tag pixels long
(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((scale (/ (vec-length (vec-from-to ?ulhc ?urhc)) 5)))
      (Claim-derived this this 'scale scale))
    (Remember this this 'ball-pos ball-pos)
    (Remember this this 'ball-vel ball-vel)
    (Remember this this 'left-paddle-y left-paddle-y)
    (Remember this this 'right-paddle-y right-paddle-y)
    (Remember this this 'left-paddle-v left-paddle-v)
    (Remember this this 'right-paddle-v right-paddle-v)
    (let* ((margin (/ 8 5)) (dx (/ 101 5)) (dy (/ 69 5)))
      (Wish-derived this this 'has-region-from-tag-unrotated
       `(pong 0 ,margin ,dx ,margin 0 ,dy ,dx ,dy))))

; time only moves forward if we have two paddles
; otherwise ball is suspended midair waiting for paddle
; paddle-velocity is claimed based on virtual and real paddle
; pos/vel are relative units defined in pong-area pixel space (51x31)
(When ((,this scale ,?scale)
       (,this pong-paddle (left . ,?y1))
       (,this pong-paddle (right . ,?y2))
       (,this ball-pos ,?ball-pos)
       (,this ball-velocity ,?ball-vel)
       (,this left-paddle-y ,?lefty)
       (,this left-velocity ,?leftv)
       (,this right-paddle-y ,?righty)
       (,this right-velocity ,?rightv))
 do (
; todo: update positions based on velocity and boundaries
(display 'todo)
))

(When ((,this pong-paddle (left . ,?attractorY))
       (,this left-paddle-y ,?paddleY)
       (,this left-paddle-v ,?paddleV))
 do
; todo: update paddle velocity based on previous
(let ((y (inexact->exact (round ?attractorY))))
  (set! left-paddle-y y)
  (Remember this this 'left-paddle-y y))
)

(When ((,this pong-paddle (right . ,?attractorY))
       (,this right-paddle-y ,?paddleY)
       (,this right-paddle-v ,?paddleV))
 do
; todo: update paddle velocity based on previous
(let ((y (inexact->exact (round ?attractorY))))
  (set! right-paddle-y y)
  (Remember this this 'right-paddle-y y))
)

(When ((,this has-region (pong ,?rotation ,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,this scale ,?scale)
       (,this ball-pos ,?ball-pos)
       (,this left-paddle-y ,?lefty)
       (,this right-paddle-y ,?righty))
 do (let* ((center (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5))))
           (cx (car center)) (cy (cdr center))
           (ball-tl (vec->ints (vec-add ?ulhc (vec-mul ball-pos ?scale))))
           (ball-br (vec->ints (vec-add ?ulhc (vec-add (vec-mul ball-pos ?scale) (cons ?scale ?scale)))))
           (left-tl (vec->ints (vec-add ?ulhc (vec-mul (cons 2 ?lefty) ?scale))))
           (left-br (vec->ints (vec-add ?ulhc (vec-mul (cons 3 (+ ?lefty 5)) ?scale))))
           (right-tl (vec->ints (vec-add ?urhc (vec-mul (cons -3 ?righty) ?scale))))
           (right-br (vec->ints (vec-add ?urhc (vec-mul (cons -2 (+ ?righty 5)) ?scale))))
           ; m rotates back to axis-aligned with ulhc at upper left hand corner
           (minv (rotation-matrix-2d cx cy (- ?rotation) 1.0))
           (img (create-image projx projy 16))  ; 16 is 3-channel CV8U
           (mask (create-image projx projy 0))) ; 0 is 1-channel CV8U
        (draw-line img (car ?ulhc) (cdr ?ulhc) (car ?urhc) (cdr ?urhc) 255 255 255 2)
        (draw-line img (car ?urhc) (cdr ?urhc) (car ?lrhc) (cdr ?lrhc) 255 255 255 2)
        (draw-line img (car ?lrhc) (cdr ?lrhc) (car ?llhc) (cdr ?llhc) 255 255 255 2)
        (draw-line img (car ?llhc) (cdr ?llhc) (car ?ulhc) (cdr ?ulhc) 255 255 255 2)
        (draw-rectangle img (car left-tl) (cdr left-tl) (car left-br) (cdr left-br) 255 255 255 -1)
        (draw-rectangle img (car right-tl) (cdr right-tl) (car right-br) (cdr right-br) 255 255 255 -1)
        (draw-rectangle img (car ball-tl) (cdr ball-tl) (car ball-br) (cdr ball-br) 255 255 255 -1)
        (draw-rectangle mask (car ?ulhc) (cdr ?ulhc) (car ?lrhc) (cdr ?lrhc) 255 255 255 -1)
        (warp-affine img img minv projx projy)    ; rotate back
        (warp-affine mask mask minv projx projy)  ; rotate back
        (copy-from-to img projection mask)
        (free-image img)
        (free-image mask)
        (free-image minv)))
