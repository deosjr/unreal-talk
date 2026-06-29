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
  (let ((facts (dl-find (fresh-vars 1 (lambda (v) (dl-findo (get-dl) ( (,e ,attr ,v) )))))))
    (not (null? facts))))

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

; TODO: only draw params when someone wants us to draw params?
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

(Wish this 'has-parameter '(a 0 10 3))
(Wish this 'has-parameter '(b 5 25 10))
(Wish this 'has-parameter '(c -5 5 0))
