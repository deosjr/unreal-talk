(When ((time detect ,?d))
 do (let ((str (format #f "Time detect: ~dms" ?d)))
      (Wish-derived this this 'subtitled str))) 
