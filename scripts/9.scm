(When ((,?someone wishes (,?p highlighted ,?color))
       (,?p (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((r 0) (g 0) (b 0))
      (case ?color
        ((red) (set! r 255))
        ((blue) (set! b 255))
        ((green) (set! g 255))
        (else (set! r (car   ?color)) ; assume color is (r g b) list
              (set! g (cadr  ?color))
              (set! b (caddr ?color))))
      (draw-on-page ?ulhc ?urhc ?lrhc ?llhc r g b)))

(When ((,this has-region (outline ,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (draw-line projection (car ?ulhc) (cdr ?ulhc) (car ?urhc) (cdr ?urhc) 255 255 255 2)
    (draw-line projection (car ?urhc) (cdr ?urhc) (car ?lrhc) (cdr ?lrhc) 255 255 255 2)
    (draw-line projection (car ?lrhc) (cdr ?lrhc) (car ?llhc) (cdr ?llhc) 255 255 255 2)
    (draw-line projection (car ?llhc) (cdr ?llhc) (car ?ulhc) (cdr ?ulhc) 255 255 255 2))
