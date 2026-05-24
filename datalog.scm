(include "minikanren.scm")
(use-modules (srfi srfi-9))

(define-record-type <datalog>
  (make-datalog edb idb rdb idx-entity idx-attr counter)
  datalog?
  (edb        datalog-edb)
  (idb        datalog-idb set-datalog-idb!)
  (rdb        datalog-rdb set-datalog-rdb!)
  (idx-entity datalog-idx-entity)
  (idx-attr   datalog-idx-attr)
  (counter    datalog-counter set-datalog-counter!))

(define (make-new-datalog)
  (make-datalog
   (make-hash-table)   ; edb
   (make-hash-table)   ; idb
   (make-hash-table)   ; rdb
   (make-hash-table)   ; idx-entity
   (make-hash-table)   ; idx-attr
   0))                ; counter

(define (dl-next-id! dl)
  (let ((n (+ 1 (datalog-counter dl))))
    (set-datalog-counter! dl n)
    n))

(define (dl-assert! dl entity attr value)
  (hash-set! (datalog-edb dl) (list entity attr value) #t)
  (dl-update-indices! dl (list entity attr value)))

; sibling of dl-assert! for facts derived during fixpoint: writes to the
; IDB (so the next dl-fixpoint! reset can retract them) and to the
; indices (so dl-findo can match them in subsequent iterations). No EDB
; write — derived facts live for one fixpoint and are re-derived if
; their preconditions still hold.
(define (dl-assert-derived! dl entity attr value)
  (hash-set! (datalog-idb dl) (list entity attr value) #t)
  (dl-update-indices! dl (list entity attr value)))

; Semi-naive bookkeeping: every fact added to the indices pushes its attr
; into *delta-attrs* for the current iteration. The fixpoint loop reads
; *delta-attrs* at the end of iteration k to decide which rules to
; re-evaluate in iteration k+1 — rules whose body attrs don't intersect
; delta are guaranteed to produce no new outputs and can be skipped.
(define *delta-attrs* (make-hash-table))
(define (reset-delta-attrs!)
  (set! *delta-attrs* (make-hash-table)))

(define (dl-update-indices! dl tuple)
   (let ((entity (car tuple))
         (attr (cadr tuple))
         (idx-entity (datalog-idx-entity dl))
         (idx-attr (datalog-idx-attr dl)))
     (let ((m (hash-ref idx-entity entity #f)))
       (if m (hash-set! m tuple #t)
         (let ((new (make-hash-table)))
           (hash-set! idx-entity entity new)
           (hash-set! new tuple #t))))
     (let ((m (hash-ref idx-attr attr #f)))
       (if m (hash-set! m tuple #t)
         (let ((new (make-hash-table)))
           (hash-set! idx-attr attr new)
           (hash-set! new tuple #t))))
     (hash-set! *delta-attrs* attr #t)))

(define-syntax dl-record!
   (syntax-rules ()
     ((_ dl type (attr value) ...) (let ((id (dl-next-id! dl)))
       (dl-assert! dl id (list type attr) value) ... id))))

; goal looks like: (fresh-vars 3 (lambda (q ?x ?y) (equalo q ?x) (dl_findo ( (,?x '(car speed) 4) ))))
(define (dl-find goal) (runf* goal)) ; goal already encapsulated dl

(define-syntax dl-findo
  (syntax-rules ()
    ((_ dl (m ...)) (conj+ (dl-findo_ dl `m) ... ))))

(define (dl-findo_ dl m)
   (fresh (x y entity attr db)
   (conso entity x m)
   (conso attr y x)
     (conde
       [(boundo entity) (lookupo (datalog-idx-entity dl) entity db) (membero m db)]
       [(unboundo entity) (boundo attr) (lookupo (datalog-idx-attr dl) attr db) (membero m db)] )))

; compiles the rule to a goal function
; here we need to find the ?vars and assert #`(fresh-vars #,num-vars (lambda (#,@vars) (conj (equalo q #,head) (dl_findo #,@body))))
(define-syntax dl-rule!
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

  (syntax-case stx (:-)
    ((_ dl (head hx hy) :- (body bx by) ...)
     (let* ((datums (syntax->datum #'((hx head hy) (bx body by) ...)))
            (head-datum (car datums))
            (body-datums (cdr datums))
            (vars (remove-duplicates (collect-vars datums)))
            (numvars (+ 1 (length vars)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (replaced-head (replace-symbols head-datum sym->gen))
            (replaced-body (replace-symbols body-datums sym->gen))
            ; The attr in each body cond is the *first* element (the
            ; relation name in dl-rule! syntax). If any is a logic var
            ; we don't know what it'll bind to at runtime, so mark the
            ; whole rule as 'any (always-eligible).
            (body-attrs (syntax->datum #'(body ...)))
            (any-var? (let loop ((as body-attrs))
                        (cond ((null? as) #f)
                              ((symbol-with-question-mark? (car as)) #t)
                              (else (loop (cdr as))))))
            (rule-attrs-datum (if any-var? 'any body-attrs))
            ; Wrap as syntax so the #, splice produces a real syntax
            ; object (otherwise the expander complains about raw
            ; symbols leaking into the macro output).
            (rule-attrs (datum->syntax stx rule-attrs-datum)))
       #`(dl-assert-rule! dl (fresh-vars #,numvars
           (lambda (q #,@gens)
             (conj (equalo q `#,replaced-head)
                   (dl-findo dl #,replaced-body) ))) '#,rule-attrs))))))

; Store rule -> attrs (list of attrs the body conditions match on, or
; the symbol 'any when at least one body condition has an unknown attr
; — e.g. a logic-variable in the attr position). Semi-naive uses this
; to skip rules whose preconditions can't have changed.
(define (dl-assert-rule! dl rule attrs)
  (hash-set! (datalog-rdb dl) rule attrs))

(define (any-in-delta? attrs delta)
  (let loop ((as attrs))
    (cond ((null? as) #f)
          ((hash-ref delta (car as) #f) #t)
          (else (loop (cdr as))))))

(define (rules-to-evaluate dl prev-delta)
  (if (not prev-delta)
      (hashtable-keys (datalog-rdb dl))
      (let ((eligible '()))
        (hash-for-each
          (lambda (rule attrs)
            (when (or (eq? attrs 'any)
                      (any-in-delta? attrs prev-delta))
              (set! eligible (cons rule eligible))))
          (datalog-rdb dl))
        eligible)))

(define (dl-fixpoint! dl)
  (for-each (lambda (fact) (dl-retract! dl fact)) (hashtable-keys (datalog-idb dl)))
  (set-datalog-idb! dl (make-hash-table))
  (reset-delta-attrs!)
  (dl-fixpoint-iterate dl #f))

(define (dl-fixpoint-iterate dl prev-delta)
  (let ((rules (rules-to-evaluate dl prev-delta)))
    (if (null? rules) #t
        (begin
          (reset-delta-attrs!)   ; this iter's deltas accumulate here
          (let* ((facts (par-map (lambda (rule) (dl-apply-rule dl rule)) rules))
                 (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hash-table)))
                 (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
            (set-extend! (datalog-idb dl) new)
            (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
            (if (not (null? new)) (dl-fixpoint-iterate dl *delta-attrs*)))))))

(define (dl-apply-rule dl rule)
  (dl-find rule))

; todo: remove from edb? doesn't matter atm
(define (dl-retract! dl tuple) 
   (let ((entity (car tuple))
         (attr (cadr tuple))
         (idx-entity (datalog-idx-entity dl))
         (idx-attr (datalog-idx-attr dl)))
     (let ((m (hash-ref idx-entity entity #f)))
       (if m (hash-remove! m tuple)))
     (let ((m (hash-ref idx-attr attr #f)))
       (if m (hash-remove! m tuple)))))

(define (dl-retract-rule! dl rule) 
  (hash-remove! (datalog-rdb dl) rule))

#| HELPER FUNCTIONS |#
(define (foldl f l acc)
   (if (null? l) acc
     (foldl f (cdr l) (f (car l) acc))))

(define (conso a b l) (equalo (cons a b) l))
(define (boundo v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) mzero (unit s/c)))
        (unit s/c))))
(define (unboundo v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) (unit s/c) mzero))
        mzero)))
(define (lookupo m key value)
    (lambda (s/c)
      (let* ((k (if (var? key) (walk key (car s/c)) key))
             (v (hash-ref m k #f)))
       (if v ((equalo value (hashtable-keys v)) s/c) mzero))))

(define (membero x l)
   (fresh (a d)
     (conso a d l)
     (conde
       [(equalo a x)]
       [(membero x d)])))

(define (set-extend! m keys)
  (for-each (lambda (key)
    (hash-set! m key #t))
  keys) m)

(define (set-difference a b)
  (let ((m (make-hash-table)))
    (for-each (lambda (key)
      (if (not (hash-ref b key #f)) (hash-set! m key #t)))
    (hashtable-keys a)) m))

(define (hashtable-keys ht)
  (hash-fold (lambda (k v acc) (cons k acc)) '() ht))

;(dl-record 'car ('speed 4) ('smth 5))
; goal looks like: (fresh-vars 3 (lambda (q ?x ?y) (equalo q ?x) (dl_findo ( (,?x '(car speed) 4) ))))
;(define test2 (dl_find (fresh-vars 3 (lambda (q ?x ?y) (conj (equalo q `,?y) (dl_findo ( (,?x (car speed) 4) (,?x (car smth) ,?y) )))))))
; there could be a separate macro rewriting the below to the above
;(define test2 (dl_find ,?y where (,?x (car speed) 4) (,?x (car smth) ,?y) ))

#|
(define dl (make-new-datalog))
(define a (dl-record! dl 'vertex))
(define b (dl-record! dl 'vertex))
(define c (dl-record! dl 'vertex))
(define d (dl-record! dl 'vertex))
(define e (dl-record! dl 'vertex))
(define (dl-edge x y) (dl-assert! dl x 'edge y))
(dl-edge a c)
(dl-edge b a)
(dl-edge b d)
(dl-edge c d)
(dl-edge d a)
(dl-edge d e)
(dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?y))
(dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))
(dl-fixpoint! dl)
;(define test2 (dl_find ,?id where (,?id reachable ,?id)))
(define test2 (dl-find (fresh-vars 1 (lambda (?id) (dl-findo dl ( (,?id reachable ,?id) ))))))
(display test2) ; expect a permutation of (1 3 4)
|#
