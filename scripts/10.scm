(When ((time scm ,?d))
 do (let ((str (format #f "Time SCM: ~dms" ?d)))
      (Wish-derived this this 'subtitled str))) 
