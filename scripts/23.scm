; Here's a hack to remove: find-all facts with a certain shape
(define (any-facts? e attr)
  (let ((facts (dl-find (fresh-vars 1 (lambda (v) (dl-findo dl ( (,e ,attr ,v) )))))))
    (not (null? facts))))

(define (param-id pid name) `(param ,pid ,name))

; TODO: multiple parameter regions, need to aggregate and space them out
(When ((?someone wishes (?p has-parameter (?name ?min ?max ?default))) 
       (?p (region tag) ?r)
       (?r (region points) ?pts))
 do (if (not (any-facts? ?p ?name)) (Remember ?p ?p ?name ?default))
    (let ((pname (param-id ?p ?name)))
      (Claim ?p 'parameter pname)
      (Claim pname '(parameter name) ?name)
      (Claim pname '(parameter min) ?min)
      (Claim pname '(parameter max) ?max)
      (Claim pname '(parameter default) ?default)
      (Wish ?p `(region ,pname right) `(,?pts 2 1 1 0))))

; we send param-id in the wish to get a region
; now we can relate region-id to parameter id
(When ((?r (region name) (param ?p ?name)))
 do (Claim ?r '(region name) 'parameter)
    (Claim ?p `(region parameter) ?r)
    (Claim ?r '(region parameter) (param-id ?p ?name)))

(When ((?p (region parameter) ?r)
       (?r (region unrotated) (?ulhc ?urhc ?llhc ?lrhc))
       (?r (region rotation) ?rotation)
       (?r (region parameter) ?paramid)
       (?paramid (parameter name) ?pname)
       (?p ?pname ?value))
 do (let* ((dx (- (car ?lrhc) (car ?ulhc)))
           (dy (- (cdr ?lrhc) (cdr ?ulhc)))
           (text (list (format #f "~a: ~a" ?pname ?value)))
           (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
        (draw-text text img (cons 0 0) (cons dx dy) #:color '(255 255 255))
        (draw-mat-onto-region-opaque img projection ?rotation ?ulhc ?lrhc)
        (free-image img)))

;;=======================================================================

(Wish this 'has-parameter '(value 0 10 3))

; todo: subtitle takes region?
(When ((this value ?v)) do (Wish this 'subtitled (number->string ?v))) 
