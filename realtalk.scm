(include "datalog.scm")

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)

; known bugs:
; - derived Claim/Wish is not supported in When macro, 'this' keyword not available
; --> see below, inject 'this' explicitly in embedded Claim/Wish. Disallow nested When rules
; - derived Claim/Wish is not supported in When macro, behaviour should be different
; --> When macro replaces Claim/Wish with DerivedClaim/DerivedWish?
; FIX for both (temporarily): Claim-derived

; Remember/Forget can store state beyond a page's lifetime.
; For now we will use an in-memory db so state is still scoped to RealTalkOS lifetime
; https://dynamicland.org/archive/2020/Memories
; "The remembered statements on an object are only there when the object itself is there and running"
; Values are unique per key, for now. Replacing is automatic.
; Page-local vars are still a thing too, they are just less reliable since detection isn't 100% stable

(define *procs* (make-hash-table))
(define *rule-procs* (make-hash-table))

(define dl (make-new-datalog))
(define (get-dl) dl)

; these are pointers to cv::Mat that are sent by Golang
(define webcam #f)
(define projection #f)
(define webcam->projection #f)
(define projection->webcam #f)
; these are dimensions (in pixels) of the projection image
(define projx #f)
(define projy #f)
(define (init-images imgptr projptr m x y)
  (set! webcam imgptr)
  (set! projection projptr)
  (set! webcam->projection m)
  (set! projection->webcam (matrix-invert m)) ; todo: free
  (set! projx x)
  (set! projy y))

(define *background-pages* '())
(define *pages-in-scene* (make-hash-table))
(define *pages-in-scene-prev* (make-hash-table))

(define (update-global-page-registry id)
  (hash-set! *pages-in-scene* id #t))

(define (get-new-pages)
  (filter (lambda (id) (not (hash-ref *pages-in-scene-prev* id #f)) )
    (hashtable-keys *pages-in-scene*)))

(define (get-removed-pages)
  (filter (lambda (id) (not (hash-ref *pages-in-scene* id #f)) )
    (hashtable-keys *pages-in-scene-prev*)))

(define (pages-found pages key)
  (fill-image projection 0 0 0) ;; fill black
  (for-each (lambda (page)
    (let ((id (car page))
          (points (cadr page))
          (rotation (caddr page)))
      (update-global-page-registry id)
      (update-page-geometry id points rotation))) pages)
  (let ((new-pages (get-new-pages))
        (removed-pages (get-removed-pages)))
    (for-each (lambda (id) (page-moved-onto-table id)) new-pages)
    (for-each (lambda (id) (page-moved-from-table id)) removed-pages)
    (for-each (lambda (id) (forget-all id)) removed-pages))
  (retract-memories)
  (let ((pageids (hashtable-keys *pages-in-scene*)))
    (for-each (lambda (id) (assert-memories id)) pageids))
  (set! *pages-in-scene-prev* *pages-in-scene*)
  (set! *pages-in-scene* (make-hash-table))
  (set! *forget* '())
  (assert-key key)
  (assert-time)
  (dl-fixpoint! (get-dl)))

(define-syntax make-page-code
  (lambda (stx)
    (syntax-case stx ()
      ((_ body ...)
       (with-syntax ((this (datum->syntax stx 'this)))
         #'(lambda (this) body ...))))))

(define-syntax Claim
  (lambda (stx)
    (syntax-case stx ()
      ((_ id attr value)
       (with-syntax ((this (datum->syntax stx 'this)))
         #'(begin
             (dl-assert! (get-dl) this 'claims (list id attr value))
             (dl-assert! (get-dl) id attr value)))))))

(define-syntax Wish
  (lambda (stx)
    (syntax-case stx ()
      ((_ id attr value)
       (with-syntax ((this (datum->syntax stx 'this)))
       #'(dl-assert! (get-dl) this 'wishes (list id attr value)))))))

(define-syntax When
  (lambda (stx)
    (define (symbol-with-question-mark? s)
      (and (symbol? s)
           (let ((str (symbol->string s)))
             (and (positive? (string-length str))
                  (char=? (string-ref str 0) #\?)))))

  (define (collect-vars datum)
        (cond
          [(symbol? datum)
           (if (symbol-with-question-mark? datum) (list datum) '())]
          [(pair? datum)
             (append (collect-vars (car datum))
                     (collect-vars (cdr datum)))]
          [else '()]))

  (define (remove-duplicates syms)
      (define seen '())
      (define (unique s)
        (let ((d (syntax->datum s)))
          (if (member d seen) #f
              (begin (set! seen (cons d seen)) #t))))
      (filter unique syms))

  (define (replace-symbols datum sym->gen)
    (cond
      [(symbol? datum)
       (let ((mapped (assoc datum sym->gen)))
         (if mapped (cdr mapped) (datum->syntax stx datum)))]
      [(pair? datum)
       (cons (replace-symbols (car datum) sym->gen)
             (replace-symbols (cdr datum) sym->gen))]
      [else (datum->syntax stx datum)]))

  (syntax-case stx (do)
    ((_ ((cx condition cy) ...) do statement ...)
       (with-syntax ((this (datum->syntax stx 'this)))
         (let* ((datums (syntax->datum #'((cx condition cy) ...)))
            (vars (remove-duplicates (collect-vars datums)))
            (numvars (+ 1 (length vars)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (st-datums (syntax->datum #'(statement ...)))
            (replaced-statements (replace-symbols st-datums sym->gen))
            (replaced-conditions (replace-symbols datums sym->gen)))
       #`(let* ((code `,(lambda (this #,@gens) (begin #,@replaced-statements)))
                (code-name (gensym))
                (rule (fresh-vars #,numvars (lambda (q #,@gens)
                          (conj (equalo q (list this 'code (cons code-name (list #,@gens))))
                                (dl-findo (get-dl) #,replaced-conditions))))))
             (hash-set! *rule-procs* code-name code)
             (dl-assert! (get-dl) this 'rules rule)
             (dl-assert-rule! (get-dl) rule))))))))

; derived Claim, ie Claim used within a When. Its facts are more fleeting than top-level Claims.
; todo: to be substituted automatically for a Claim within a When body
(define (Claim-derived this id attr value)
  (hash-set! (datalog-idb (get-dl)) `(,this claims (,id ,attr ,value)) #t)
  (hash-set! (datalog-idb (get-dl)) `(,id ,attr ,value) #t)
  (dl-assert! (get-dl) this 'claims (list id attr value))
  (dl-assert! (get-dl) id attr value))

(define (Wish-derived this id attr value)
  (hash-set! (datalog-idb (get-dl)) `(,this wishes (,id ,attr ,value)) #t)
  (dl-assert! (get-dl) this 'wishes (list id attr value)))

; redefine dl-fixpoint! injecting code execution as result of rules
(define (dl-fixpoint! dl)
  (for-each (lambda (fact) (dl-retract! dl fact)) (hashtable-keys (datalog-idb dl)))
  (set-datalog-idb! dl (make-hash-table))
  (dl-fixpoint-iterate dl))

(define (dl-fixpoint-iterate dl)
  (let* ((facts (map (lambda (rule) (dl-apply-rule dl rule)) (hashtable-keys (datalog-rdb dl))))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hash-table)))
         (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
    (set-extend! (datalog-idb dl) new)
    (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
    ; result of dl_apply_rule should be a tuple (this 'code (proc . args))
    (for-each (lambda (c)
      (let ((this (car c))
            (proc (caaddr c))
            (args (cdaddr c)))
         (apply (hash-ref *rule-procs* proc #f) this args))) new)
    (if (not (null? new)) (dl-fixpoint-iterate dl))))

(define (make-page-id) (dl-record dl 'page))

(define (add-page proc)
  (let* ((pid (dl-record! dl 'page ('code proc))))
    (hash-set! *procs* pid proc)
    pid))

; make page dimensions known in datalog
;(define (update-page-geometry pid ulhc urhc llhc lrhc rotation)
(define (update-page-geometry pid points rotation)
    (retract-page-geometry pid)
    (dl-assert! dl pid '(page points) points)
    (dl-assert! dl pid '(page rotation) rotation))

(define (retract-page-geometry pid)
  (let (( points (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page points) ,x) ))))))
        ( rotation (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page rotation) ,x) )))))))
    (if (not (null? points)) (dl-retract! dl `(,pid (page points) ,(car points))))
    (if (not (null? rotation)) (dl-retract! dl `(,pid (page rotation) ,(car rotation))))))

; only run page code when newly in bounds of table
(define (page-moved-onto-table pid)
  (execute-page pid))

; then retract all 'this claims x' and 'this rules x' from dl-db when newly out of table bounds
(define (page-moved-from-table pid)
  (retract-page-geometry pid)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid claims ,x) ))))))
        ( wishes (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid wishes ,x) ))))))
        ( rules  (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid rules ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl claim)) claims)
    (for-each (lambda (claim) (dl-retract! dl `(,pid claims ,claim))) claims)
    (for-each (lambda (wish) (dl-retract! dl `(,pid wishes ,wish))) wishes)
    (for-each (lambda (rule) (dl-retract-rule! dl rule)) rules)
    (for-each (lambda (rule) (dl-retract! dl `(,pid rules ,rule))) rules)))

; note: guile scheme gettimeofday returns a pair of seconds and microseconds in unix epoch 
(define (assert-time)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time now ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time now ,claim))) claims))
  (dl-assert! dl 'time 'now (gettimeofday)))

; if key == -1 then no key was pressed
(define (assert-key key)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (key down ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(key down ,claim))) claims))
  (if (not (= key -1)) (dl-assert! dl 'key 'down key)))

(define (read-page-code id)
  (call-with-input-file (format #f "scripts/~d.scm" id) (lambda (port)
    (get-string-all port)) #:encoding "utf-8"))

(define (load-page id)
  (let* ((str (read-page-code id))
         (proc (eval-string (format #f "(make-page-code ~a)" str))))
    (dl-assert! (get-dl) id '(page code) str)
    (hash-set! *procs* id proc)))

; todo: once a background page is physically present on the table,
; it unloads once gone and takes all its effects with it :)
(define (load-background-page id)
  (load-page id)
  (page-moved-onto-table id)
  (set! *background-pages* (cons id *background-pages*)))

(define (execute-page pid)
  (let ((proc (hash-ref *procs* pid #f)))
    (if proc (proc pid))))

(define *memories* (make-hash-table))
(define *forget* '()) ; stages memories to forget; cleaned up each iteration

(define (Remember on id key value)
  (Forget on id key) ; always replace
  (hash-set! *memories* (list on id key) value))

(define (Forget on id key)
  (let* ((memkey (list on id key))
         (value (hash-ref *memories* memkey))
         (dbkey (list id key value)))
  (set! *forget* (cons dbkey *forget*))
  (hash-remove! *memories* memkey)))

(define (memories-on on)
  (filter (lambda (mem) (eq? (car mem) on)) (hashtable-keys *memories*)))

(define (forget-all on)
  (let ((to-forget (map (lambda (mem)
    (let ((id (cadr mem))
          (key (caddr mem))
          (value (hash-ref *memories* mem #f)))
      (list id key value))) (memories-on on))))
  (set! *forget* (append *forget* to-forget))))

(define (assert-memories pid)
  (let ((memories (memories-on pid)))
    (for-each (lambda (mem)
      (let ((id (cadr mem))
            (key (caddr mem))
            (value (hash-ref *memories* mem #f)))
        (dl-assert! (get-dl) id key value)))
      memories)))

(define (retract-memories)
  (for-each (lambda (dbkey)
    (dl-retract! (get-dl) dbkey)) *forget*))
