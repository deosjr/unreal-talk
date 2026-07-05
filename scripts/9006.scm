(define (param-id pid name) `(param ,pid ,name))

(When ((?someone wishes (?p has-parameter (?name ?min ?max ?default))) 
       (?p (region tag) ?r)
       (?r (region points) ?pts))
 do (if (not (any-facts? ?p ?name)) (Remember ?p ?p ?name ?default))
    (let ((paramid (param-id ?p ?name)))
      (Claim ?p 'parameter paramid)
      (Claim paramid '(parameter name) ?name)
      (Claim paramid '(parameter min) ?min)
      (Claim paramid '(parameter max) ?max)
      (Claim paramid '(parameter default) ?default)))

; Here's a hack to remove: find-all facts with a certain shape
(define (any-facts? e attr)
  (not (null? (dl-query (get-dl) ((,e ,attr ?v)) ?v))))

; we send param-id in the wish to get a region
; now we can relate region-id to parameter id
(When ((?r (region name) (param ?p ?name)))
 do (Claim ?r '(region name) 'parameter)
    (Claim ?p `(region parameter) ?r)
    (Claim ?r '(region parameter) (param-id ?p ?name)))

(define (sort-params params)
  (sort params (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(Collect ((?p parameter ?paramid)
          (?paramid (parameter name) ?name)
          (?p (region tag) ?r)
          (?r (region points) ?pts))
 emit (list ?p ?pts ?name)
 as   parameters
 do (let ((page-points (make-hash-table))
          (page-params (make-hash-table)))
      ; collect our data in hashmaps
      (for-each (lambda (x)
        (let ((p (car x))
              (pts (cadr x))
              (name (caddr x)))
          (hash-set! page-points p pts)
          (hash-set! page-params p
            (cons name (hash-ref page-params p '()))))) parameters)
      ; wish a param region on pid for each parameter
      (hash-for-each (lambda (pid params)
        (let ((sorted (sort-params params))
              (pts (hash-ref page-points pid #f)))
          (for-each (lambda (param i)
            (let ((paramid (param-id pid param))
                  (y (* (+ i 1) 2)))
              (Wish pid `(region ,paramid right) `(,pts 2 1 1 ,y))))
            sorted
            (iota (length sorted)))))
        page-params)))

; NOTE: this syntax only works with some assumptions on ?e ?attr and ?v
; TODO: does this need to be deduped for multiple wishers or does Claim do that?
; I think the Claim already takes care of that tbh
(When ((?someone wishes (parameters are drawn)))
 do (Claim this 'draws 'parameters))

(When ((this draws parameters)
       (?p (region parameter) ?r)
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
