(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "datalog")

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((res (get-url (string-append urlpref topic))))
      (if res
        (let* ((sxml (parse-ssax res))
               (matched ((sxpath '(// p)) sxml))
               (first (car matched))
               (str (sxml->string first)))
          (Wish-derived this this 'subtitled str))
        (Wish-derived this this 'labeled "LOADING"))))
