(Wish this 'has-whiskers #t)

(When ((this points-at ?r)
       (?r (region points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let ((ptr (bytevector->pointer (points->bytevector ?ulhc ?urhc ?lrhc ?llhc))))
      (fill-poly projection ptr 4 255 0 0)))
