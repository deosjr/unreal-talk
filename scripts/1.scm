; this script was made for a 9x9cm tag in the upper lefthand corner of an A4 paper

(Wish this 'has-whiskers #t)

(define line-num 1)
(define cursor-x 0)

(When ((,this line-num ,?line-num)) ; from Remember
 do (set! line-num ?line-num))
(When ((,this cursor-x ,?x))
 do (set! cursor-x ?x))

; todo: Remember current script size, don't go out of bounds
(When ((key down 106)) ; j
 do (Forget this this 'line-num)
    (Remember this this 'line-num (+ line-num 1)))

(When ((key down 107)) ; k
 do (if (> line-num 1)
      (let ((new-num (- line-num 1)))
        (Forget this this 'line-num)
        (Remember this this 'line-num new-num))))

(When ((key down 104)) ; h
 do (if (> cursor-x 0)
      (let ((newx (- cursor-x 1)))
        (Forget this this 'cursor-x)
        (Remember this this 'cursor-x newx))))

(When ((key down 108)) ; l
 do (Forget this this 'cursor-x)
    (Remember this this 'cursor-x (+ cursor-x 1)))

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((diagonal (vec-from-to ?lrhc ?ulhc))
           ; inner dimensions of 9x9 tag at 1cm per pixel: 5x5cm
           ; diagonal is sqrt(50) and to page topleft is sqrt(32)
           ; roughly 7.07 : 5.66
           (topleft (vec-add ?ulhc (vec-mul diagonal (/ 5.66 7.07))))
           ; a4 in cm: 21 x 29.7
           (inner-right-vec (vec-mul (vec-from-to ?ulhc ?urhc) 0.2)) ; normalized to be 1cm/1px long
           (inner-down-vec (vec-mul (vec-from-to ?ulhc ?llhc) 0.2)) ; normalized to be 1cm/1px long
           (rightvec (vec-mul inner-right-vec 21))
           (downvec (vec-mul inner-down-vec 29.7))
           (topright (vec-add topleft rightvec))
           (bottomleft (vec-add topleft downvec))
           (bottomright (vec-add bottomleft rightvec))
           (tlx (coord->int (car topleft))) (tly (coord->int (cdr topleft)))
           (trx (coord->int (car topright))) (try (coord->int (cdr topright)))
           (blx (coord->int (car bottomleft))) (bly (coord->int (cdr bottomleft)))
           (brx (coord->int (car bottomright))) (bry (coord->int (cdr bottomright)))
           (editorulhc (vec-add (vec-add ?llhc (vec-mul inner-right-vec (- 2 ))) (vec-mul inner-down-vec 3)))
           (editorurhc (vec-add editorulhc (vec-mul inner-right-vec 17)))
           (editorlrhc (vec-add editorurhc (vec-mul inner-down-vec 16)))
           (editorllhc (vec-add editorulhc (vec-mul inner-down-vec 16)))
           (etlx (coord->int (car editorulhc))) (etly (coord->int (cdr editorulhc)))
           (etrx (coord->int (car editorurhc))) (etry (coord->int (cdr editorurhc)))
           (eblx (coord->int (car editorllhc))) (ebly (coord->int (cdr editorllhc)))
           (ebrx (coord->int (car editorlrhc))) (ebry (coord->int (cdr editorlrhc))))
      (draw-line projection tlx tly trx try 255 255 255 2)
      (draw-line projection trx try brx bry 255 255 255 2)
      (draw-line projection brx bry blx bly 255 255 255 2)
      (draw-line projection blx bly tlx tly 255 255 255 2)
      (Claim-derived this this 'has-region (list (cons etlx etly) (cons etrx etry) (cons eblx ebly) (cons ebrx ebry)))))

; x and y are lower left corner in aab. rotation is left to the caller.
; caller is also assumed to draw onto a poly-fill, ie mask includes text already.
(define (draw-editor-line img str x y font scale r g b thickness)
    (put-text img (string->pointer str) x y font scale r g b thickness))  ; draw color to 3-channel img

(define (draw-editor-lines img mask lines ulhc lrhc char-width line-height font scale r g b thickness)
  (let* ((ulhcx (coord->int (car ulhc))) (ulhcy (coord->int (cdr ulhc)))
         (lrhcx (coord->int (car lrhc))) (lrhcy (coord->int (cdr lrhc)))
         (line-y (+ ulhcy (* line-height (- line-num 1))))
         (cx (+ ulhcx (* char-width cursor-x)))
         (ytotal (- lrhcy ulhcy)))
    (draw-rectangle img ulhcx ulhcy lrhcx lrhcy 0 0 255 -1)
    (draw-rectangle img ulhcx line-y lrhcx (+ line-y line-height) 100 100 255 -1)
    (draw-rectangle img cx line-y (+ cx char-width) (+ line-y line-height) 150 150 255 -1)
    (draw-rectangle mask ulhcx ulhcy lrhcx lrhcy 255 255 255 -1)
    (let loop ((lst lines) (y 1))
      (let ((dy (* y line-height)))
        (if (and (< dy ytotal) (not (null? lst)))
          (let ((line (car lst)))
            (draw-editor-line img line ulhcx (+ ulhcy dy) 0 0.5 255 255 255 1)
            (loop (cdr lst) (+ y 1))))))
))

(When ((,this has-region (,?ulhc ,?urhc ,?llhc ,?lrhc))
       (,this (page rotation) ,?rotation) ; clockwise rotation
       (,this points-at ,?p)
       (,?p (page code) ,?str))
 do (let* ((center (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5)))
           (cx (inexact->exact (round (car center)))) (cy (inexact->exact (round (cdr center))))
           (textsize (text-size "gh" 0 0.5 1)) ;gh give upper/lower bounds for line
           (charwidth (/ (car textsize) 2)) ; assumes mono!
           (height (+ (cadr textsize) 8)) ; 8 padding pixels
           (lines (string-split ?str #\newline))
           ; m rotates back to axis-aligned with ulhc at upper left hand corner
           (m (rotation-matrix-2d cx cy ?rotation 1.0)) ; counter-clockwise rotation!
           (minv (rotation-matrix-2d cx cy (- ?rotation) 1.0))
           (img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
           (mask (create-image 1280 720 0)) ; 0 is 1-channel CV8U
           (in-pts (bytevector->pointer (points->bytevector ?ulhc ?urhc ?lrhc ?llhc)))
           (out-pts (bytevector->pointer (make-bytevector (* 8 4)))))
      (transform in-pts 4 m out-pts) ; rotate to axis-aligned
      (let* ((aabb-pts (pts->coords out-pts 4))
             (aabb-ulhc (car aabb-pts))
             (aabb-lrhc (caddr aabb-pts)))
        (draw-editor-lines img mask lines aabb-ulhc aabb-lrhc charwidth height 0 0.5 255 255 255 1)
        (warp-affine img img minv 1280 720)    ; rotate back
        (warp-affine mask mask minv 1280 720)  ; rotate back
        (copy-from-to img projection mask)
        (free-image img)
        (free-image mask)
        (free-image m)
        (free-image minv))))
