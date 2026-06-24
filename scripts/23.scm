; Here's a hack to remove: 
; find-all facts with a certain shape
(define (any-facts? e attr)
  (let ((facts (dl-find (fresh-vars 1 (lambda (v) (dl-findo dl ( (,e ,attr ,v) )))))))
    (not (null? facts))))

; todo: regions as their own entities in datalog
; so we can store different facts about them without storing one giant list
(When ((?someone wishes (?p has-parameter (?pname ?min ?max ?default))) 
       (?p (page points) ?r))
 do (if (not (any-facts? ?p ?pname)) (Remember ?p ?p ?pname ?default))
    ; todo: dislay param name and current value in param region 
    (Wish ?p '(region outline right) `(,?r 1 1 1 0)))

(Wish this 'has-parameter '(value 0 10 3))

; todo: subtitle takes region?
(When ((this value ?v)) do (Wish this 'subtitled (number->string ?v))) 

(When ((?someone wishes (?p has-parameter (?pname ?min ?max ?default))))
 do (if (any-facts? ?p ?pname) (Remember ?p ?p ?pname 42)))
