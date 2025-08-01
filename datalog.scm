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
           (hash-set! new tuple #t))))))

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
            (replaced-body (replace-symbols body-datums sym->gen)))
       #`(dl-assert-rule! dl (fresh-vars #,numvars
           (lambda (q #,@gens)
             (conj (equalo q `#,replaced-head)
                   (dl-findo dl #,replaced-body) )))))))))

(define (dl-assert-rule! dl rule)
  (hash-set! (datalog-rdb dl) rule #t))

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
    (if (not (null? new)) (dl-fixpoint-iterate dl))))

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
