; pong game

(define field-width 101)
(define field-height 61)

(define left-score 0)
(define right-score 0)

; play area is defined in tag pixel size, underneath the tag
; basis vectors are x and y of size 1 tag pixel
; the ball is one tag pixel in diameter
; paddles are 5 tag pixels long
(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((scale (/ (vec-length (vec-from-to ?ulhc ?urhc)) 5))
           ; defaults to start the game off, if none are Remembered
           (bx (/ field-width 2))
           (by (/ field-height 2))
           (ball-pos (vec-mul (cons bx by) scale))
           (ball-vel (cons 5 -5))
           (paddle-y (- (inexact->exact (round (/ field-height 2))) 2)))
      (Claim this 'scale scale)
      (Remember this this 'ball-pos ball-pos)
      (Remember this this 'ball-vel ball-vel)
      (Remember this this 'left-paddle-y paddle-y)
      (Remember this this 'right-paddle-y paddle-y))
    (let* ((margin (/ 8 5)) (dx (/ 101 5)) (dy (/ 69 5)))
      (Wish this 'has-region-from-tag-unrotated
       `(pong 0 ,margin ,dx ,margin 0 ,dy ,dx ,dy))))

; calculate _new_ iteration updates
; time only moves forward if we have two paddles
; otherwise ball is suspended midair waiting for paddle
; paddle-velocity is claimed based on virtual and real paddle
; pos/vel are relative units defined in pong-area pixel space (51x31)
(When ((this scale ?scale)
       (this pong-paddle (left . ?y1))
       (this pong-paddle (right . ?y2))
       (this ball-pos ?ball-pos)
       (this ball-vel ?ball-vel)
       (this left-paddle-y ?lefty)
       (this right-paddle-y ?righty))
 do (let* ((newpos (vec-add ?ball-pos ?ball-vel))
           (newx (car newpos))
           (newy (cdr newpos)))
      (if (or (<= newy 0) (>= newy (* ?scale field-height)))
        (Remember this this 'ball-vel (cons (car ?ball-vel) (- (cdr ?ball-vel))))
        (Remember this this 'ball-vel ?ball-vel))
      (if (or (<= newx 0) (>= newx (* ?scale field-width)))
        (begin 
          (Remember this this 'ball-pos (vec-mul (cons (/ field-width 2) (/ field-height 2)) ?scale))
          (Remember this this 'ball-vel (cons (- (car ?ball-vel)) (cdr ?ball-vel)))
        )
        (Remember this this 'ball-pos newpos))
))

(When ((this pong-paddle (left . ?attractorY))
       (this left-paddle-y ?paddleY))
 do
; todo: update paddle velocity based on previous
(let ((y (inexact->exact (round ?attractorY))))
  (Remember this this 'left-paddle-y y))
)

(When ((this pong-paddle (right . ?attractorY))
       (this right-paddle-y ?paddleY))
 do
; todo: update paddle velocity based on previous
(let ((y (inexact->exact (round ?attractorY))))
  (Remember this this 'right-paddle-y y))
)

; draw _last_ iteration updates to projection
(When ((this (region pong) (?rotation ?ulhc ?urhc ?llhc ?lrhc))
       (this scale ?scale)
       (this ball-pos ?ball-pos)
       (this left-paddle-y ?lefty)
       (this right-paddle-y ?righty))
 do (let* ((center (vec->ints (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5))))
           (cx (car center)) (cy (cdr center))
           (ball (vec->ints (vec-add ?ulhc ?ball-pos)))
           (left-tl (vec->ints (vec-add ?ulhc (vec-mul (cons 2 ?lefty) ?scale))))
           (left-br (vec->ints (vec-add ?ulhc (vec-mul (cons 3 (+ ?lefty 5)) ?scale))))
           (right-tl (vec->ints (vec-add ?urhc (vec-mul (cons -3 ?righty) ?scale))))
           (right-br (vec->ints (vec-add ?urhc (vec-mul (cons -2 (+ ?righty 5)) ?scale))))
           ; m rotates back to axis-aligned with ulhc at upper left hand corner
           (minv (rotation-matrix-2d cx cy (- ?rotation) 1.0))
           (img (create-image projx projy 16))  ; 16 is 3-channel CV8U
           (mask (create-image projx projy 0))) ; 0 is 1-channel CV8U
        (draw-rectangle img (car ?ulhc) (cdr ?ulhc) (car ?lrhc) (cdr ?lrhc) 255 255 255 2)
        (draw-rectangle img (car left-tl) (cdr left-tl) (car left-br) (cdr left-br) 255 255 255 -1)
        (draw-rectangle img (car right-tl) (cdr right-tl) (car right-br) (cdr right-br) 255 255 255 -1)
        (draw-circle img (car ball) (cdr ball) (inexact->exact (round (/ ?scale 2))) 255 255 255 -1)
        (draw-rectangle mask (car ?ulhc) (cdr ?ulhc) (car ?lrhc) (cdr ?lrhc) 255 255 255 -1)
        (warp-affine img img minv projx projy)    ; rotate back
        (warp-affine mask mask minv projx projy)  ; rotate back
        (copy-from-to img projection mask)
        (free-image img)
        (free-image mask)
        (free-image minv)))
