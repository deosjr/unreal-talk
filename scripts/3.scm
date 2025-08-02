(define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/")
(define topic "datalog")

(define (elem->str x)
  (if (string? x) x
    (case (car x)
      ((b) (car (last-pair x)))
      ((a) (car (last-pair x))) ; todo: assert link?
      (else ""))))

(define (p->str p)
  (string-join (map elem->str (cdr p)) ""))

(When ((,this (page points) (,?ulhc ,?urhc ,?llhc ,?lrhc)))
 do (let ((res (get-url (string-append urlpref topic))))
      (if res
        (let* ((sxml (parse-ssax res))
               (matched ((sxpath '(// p)) sxml))
               (first (car matched))
               (str (p->str first)))
          (Wish-derived this this 'subtitled str))
        (Wish-derived this this 'labeled "LOADING"))))
