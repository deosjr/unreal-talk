(Wish this 'has-whiskers #t)

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish-derived this this 'has-region-from-tag 
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish-derived this this 'has-region-from-tag-unrotated
       `(wiki ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))
