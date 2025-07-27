(Wish this 'has-whiskers #t)

(When ((,this points-at ,?p)
       (,?p (page rotation) ,?rotation)
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (draw-on-page ?ulhc ?urhc ?lrhc ?llhc 0 255 0)
    (let* ((str (number->string ?p))
           (testsize (text-size str 0 1.0 2))
           (width (car testsize))
           (height (cadr testsize))
           (baseline (caddr testsize)) ; todo: use baseline
           (temp-img (create-image 1280 720 16)) ; 16 is 3-channel CV8U
           (temp-mask (create-image 1280 720 0)) ; 0 is 1-channel CV8U
           (mid (vec-add ?ulhc (vec-mul (vec-from-to ?ulhc ?lrhc) 0.5)))
           (midx (inexact->exact (round (car mid)))) (midy (inexact->exact (round (cdr mid))))
           (m (rotation-matrix-2d midx midy (- ?rotation) 1.0)) ; assumes counter-clockwise rotation!
           (halfsize (vec-mul (cons (- width) height) 0.5))
           (textbottomleft (vec-add halfsize mid))
           (tx (inexact->exact (round (car textbottomleft)))) (ty (inexact->exact (round (cdr textbottomleft)))))
    (put-text temp-img (string->pointer str) tx ty 0 1.0 255 255 255 2)  ; draw color to 3-channel img
    (put-text temp-mask (string->pointer str) tx ty 0 1.0 255 255 255 2) ; draw white to 1-channel mask
    (warp-affine temp-img temp-img m 1280 720) ; officially doesn't support in-place modification?
    (warp-affine temp-mask temp-mask m 1280 720)
    (copy-from-to temp-img projection temp-mask)
    (free-image temp-img)
    (free-image temp-mask)
))
