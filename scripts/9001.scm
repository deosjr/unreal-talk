; This page supports text projection using wishes

; todo: use draw-mat-onto-region ?
(define* (draw-text-centered str centerpoint rotation #:key (font 0) (scale 1.0) (thickness 2))
  (let* ((testsize (text-size str font scale thickness))
         (width (car testsize))
         (height (cadr testsize))
         (baseline (caddr testsize)) ; todo: use baseline
         (strptr (string->pointer str))
         (temp-img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
         (temp-msk (create-image 1280 720 0)) ; 0 is 1-channel CV8U
         (cx (car centerpoint)) (cy (cdr centerpoint))
         (m (rotation-matrix-2d cx cy (- rotation) 1.0)) ; assumes counter-clockwise rotation!
         (halfsize (vec-mul (cons (- width) height) 0.5))
         (textbottomleft (vec->ints (vec-add halfsize centerpoint)))
         (tx (car textbottomleft)) (ty (cdr textbottomleft)))
    (put-text temp-img strptr tx ty 0 1.0 255 255 255 2)  ; draw color to 3-channel img
    (put-text temp-msk strptr tx ty 0 1.0 255 255 255 2) ; draw white to 1-channel mask
    (warp-affine temp-img temp-img m 1280 720) ; officially doesn't support in-place modification?
    (warp-affine temp-msk temp-msk m 1280 720)
    (copy-from-to temp-img projection temp-msk)
    (free-image temp-img)
    (free-image temp-msk)
    (free-image m)))

(When ((?someone wishes (?p titled ?str))
       (?p (page points) (?ulhc ?urhc ?llhc ?lrhc))
       (?p (page rotation) ?rotation))
 do (let* ((mid (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5)))
           (center (vec->ints (vec-add mid (vec-mul (vec-from-to ?llhc ?ulhc) 2.0)))))
      (draw-text-centered ?str center ?rotation)))

(When ((?someone wishes (?p subtitled ?str))
       (?p (page points) (?ulhc ?urhc ?llhc ?lrhc))
       (?p (page rotation) ?rotation))
 do (let* ((mid (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5)))
           (center (vec->ints (vec-add mid (vec-mul (vec-from-to ?ulhc ?llhc) 2.0)))))
      (draw-text-centered ?str center ?rotation)))
