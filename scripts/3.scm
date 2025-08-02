(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "datalog")

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((res (get-url (string-append urlpref topic))))
      (if res
        (Wish-derived this this 'subtitled res)
        (Wish-derived this this 'labeled "LOADING"))))
