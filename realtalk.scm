(include "datalog.scm")

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)

; known bugs:
; - derived Claim/Wish is not supported in When macro, 'this' keyword not available
; --> see below, inject 'this' explicitly in embedded Claim/Wish. Disallow nested When rules
; - derived Claim/Wish is not supported in When macro, behaviour should be different
; --> When macro replaces Claim/Wish with DerivedClaim/DerivedWish?

(define *pages* '())
(define *procs* (make-hash-table))
(define *rule-procs* (make-hash-table))

(define dl (make-new-datalog))
(define (get-dl) dl)

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
    ((_ ((condition cx cy) ...) do statement ...)
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
    (set! *pages* (cons pid *pages*))
    (hash-set! *procs* pid proc)
    pid))

; make page dimensions known in datalog
(define (update-page-geometry pid ulhc urhc llhc lrhc rotation)
    (retract-page-geometry pid)
    (dl-assert! dl pid '(page points) (list ulhc urhc llhc lrhc))
    (dl-assert! dl pid '(page rotation) rotation))

(define (retract-page-geometry pid)
  (let (( points (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page points) ,x) ))))))
        ( rotation (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page rotation) ,x) )))))))
    (if (not (null? points)) (dl-retract! dl `(,pid (page points) ,(car points))))
    (if (not (null? rotation)) (dl-retract! dl `(,pid (page rotation) ,(car rotation))))))

; todo: check if page was already recognised on table previous iteration

; only run page code when newly in bounds of table
(define (page-moved-onto-table table pid)
  (execute-page pid)
  (recalculate-pages))

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
    (for-each (lambda (rule) (dl-retract! dl `(,pid rules ,rule))) rules))
  (recalculate-pages))

; When a page is in view, its code is executed. Then when all pages have ran, dl-fixpoint runs all consequences.
; assumes a single table for now, a div with id "table"
; TODO: keep a mapping of tables->pages, and only run a page when it is newly detected on a table
; when a page is removed from the table, retract all when-rules it introduced and all claims/wishes it asserted into that tables' datalog instance.
; then remove all derived facts and run fixpoint analysis again. This way we can encapsulate state in page code!
; NOTE: there are no derived facts!!! only followup claims/rules. we can query datalog to get all claims/rules asserted by a page as we run a closure over 'this' when creating rule lambda
(define (recalculate-pages)
  ;(assert-time) ; todo
  ; todo: do we need to reset dl-idb ?
  ; currently rules execute each time a page is moved, which is not what I'd expect
  (dl-fixpoint! dl))

(define (assert-time)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (now time ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(now time ,claim))) claims)
    ;(dl-assert! dl 'now 'time (date-now)))) ; todo
  ))

(define (execute-page pid)
  ((hash-ref *procs* pid #f) pid))
