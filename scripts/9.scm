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
