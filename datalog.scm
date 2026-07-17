; ----------------------------------------------------------------------
; An EAV datalog with pattern-matching queries and delta-driven
; (semi-naive) fixpoint evaluation.
;
; Facts are ground (entity attr value) triples; the database only ever
; contains ground terms, so queries are one-way matches of a pattern
; against ground tuples — no unification, no logic-variable machinery.
; A pattern is a triple-shaped tree whose variable positions hold <pvar>
; records carrying a slot index; an environment is a plain vector of
; slots. Conjunctive queries run as a depth-first nested-loop join that
; picks the best index per condition by groundness (see run-join).
;
; Rules are <dl-rule> records: compiled condition patterns, a produce
; procedure (env -> derived tuple), the attrs the body depends on (for
; eligibility), and the source datum (so rules are printable — pages can
; introspect them). dl-fixpoint! iterates to a fixpoint semi-naively:
; each iteration only joins derivations that involve at least one fact
; from the previous iteration's delta.
;
; Surface macros:
;   (dl-query dl ((e attr v) ...) result-expr)   run a query, collect
;       result-expr per match. In conditions, ?var binds a variable
;       (in scope in result-expr), (unquote x) splices a runtime value,
;       `this` splices the page id inside page code, anything else is a
;       literal.
;   (dl-rule! dl (attr e v) :- (attr e v) ...)   assert a pure rule;
;       same condition syntax (note attr-first order, and vars may be
;       written ,?x — the unquote around a ?var is optional).
;
; The When macro in engine/dsl.scm compiles to the same <dl-rule>
; representation, with a produce that emits (this code (proc . args))
; tuples; engine/fixpoint.scm installs *fixpoint-new-facts-hook* to run
; those bodies. This file stays self-contained (datalog_test.scm loads
; it without the engine).
; ----------------------------------------------------------------------

(use-modules (ice-9 threads)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-9))

(define-record-type <datalog>
  (make-datalog edb idb rdb idx-entity idx-attr idx-eav counter)
  datalog?
  (edb        datalog-edb)
  (idb        datalog-idb set-datalog-idb!)
  (rdb        datalog-rdb set-datalog-rdb!)
  (idx-entity datalog-idx-entity)
  (idx-attr   datalog-idx-attr)
  (idx-eav    datalog-idx-eav)
  (counter    datalog-counter set-datalog-counter!))

(define (make-new-datalog)
  (make-datalog
   (make-hash-table)   ; edb
   (make-hash-table)   ; idb
   (make-hash-table)   ; rdb: rule -> #t
   (make-hash-table)   ; idx-entity
   (make-hash-table)   ; idx-attr
   (make-hash-table)   ; idx-eav
   0))                ; counter

(define (dl-next-id! dl)
  (let ((n (+ 1 (datalog-counter dl))))
    (set-datalog-counter! dl n)
    n))

; ----------------------------------------------------------------------
; Patterns, environments, matching.
; ----------------------------------------------------------------------

; A pattern variable: a slot index into the environment vector plus the
; source name (?p, ?color, ...) kept only for printing/introspection.
(define-record-type <pvar>
  (make-pvar idx name)
  pvar?
  (idx  pvar-idx)
  (name pvar-name))

; The empty-slot sentinel. A fresh list so it is eq?-unique.
(define *unbound* (list 'unbound))

(define (make-env nvars) (make-vector nvars *unbound*))

; Replace every bound pvar in P with its value; unbound pvars stay.
(define (pat-walk p env)
  (cond ((pvar? p)
         (let ((v (vector-ref env (pvar-idx p))))
           (if (eq? v *unbound*) p v)))
        ((pair? p) (cons (pat-walk (car p) env) (pat-walk (cdr p) env)))
        (else p)))

; #t when P (typically already walked) contains no unbound pvar.
(define (pat-ground? p)
  (cond ((pvar? p) #f)
        ((pair? p) (and (pat-ground? (car p)) (pat-ground? (cdr p))))
        (else #t)))

; Match pattern P against ground term V, binding pvars into ENV.
; Returns the trail (list of slots bound by THIS match — possibly empty,
; which is still success) or #f on failure. On failure any partial
; bindings are already undone. Atoms compare with equal?, so strings and
; numbers match by content.
(define (match-into! p v env)
  (let ((trail '()))
    (define (walk p v)
      (cond
        ((pvar? p)
         (let* ((i (pvar-idx p)) (cur (vector-ref env i)))
           (if (eq? cur *unbound*)
               (begin (vector-set! env i v)
                      (set! trail (cons i trail))
                      #t)
               (equal? cur v))))
        ((pair? p) (and (pair? v) (walk (car p) (car v)) (walk (cdr p) (cdr v))))
        (else (equal? p v))))
    (if (walk p v)
        trail
        (begin (for-each (lambda (i) (vector-set! env i *unbound*)) trail)
               #f))))

(define (undo-trail! env trail)
  (for-each (lambda (i) (vector-set! env i *unbound*)) trail))

; ----------------------------------------------------------------------
; Asserting facts, indices, and the delta.
; ----------------------------------------------------------------------

(define (dl-assert! dl entity attr value)
  (hash-set! (datalog-edb dl) (list entity attr value) #t)
  (dl-update-indices! dl (list entity attr value)))

; sibling of dl-assert! for facts derived during fixpoint: writes to the
; IDB (so the next dl-fixpoint! reset can retract them) and to the
; indices (so queries can match them in subsequent iterations). No EDB
; write — derived facts live for one fixpoint and are re-derived if
; their preconditions still hold.
(define (dl-assert-derived! dl entity attr value)
  (hash-set! (datalog-idb dl) (list entity attr value) #t)
  (dl-update-indices! dl (list entity attr value)))

; Semi-naive bookkeeping: every fact added to the indices lands in
; *delta*, keyed by attr, for the current iteration. The fixpoint loop
; reads it at the end of iteration k twice over: rules whose attrs don't
; intersect the delta are skipped entirely (eligibility), and eligible
; rules join with one condition at a time restricted to the delta tuples
; (each new derivation must involve at least one new fact; the rest were
; all derived in earlier iterations). This also catches EDB facts that
; rule bodies assert mid-fixpoint — they flow through
; dl-update-indices! like everything else.
(define *delta* (make-hash-table))   ; attr -> list of tuples
(define (reset-delta!)
  (set! *delta* (make-hash-table)))
(define (delta-add! attr tuple)
  (hash-set! *delta* attr (cons tuple (hash-ref *delta* attr '()))))

(define (dl-update-indices! dl tuple)
   (let ((entity (car tuple))
         (attr (cadr tuple))
         (idx-entity (datalog-idx-entity dl))
         (idx-attr (datalog-idx-attr dl))
         (idx-eav (datalog-idx-eav dl)))
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
     (let* ((e-bucket (or (hash-ref idx-eav entity #f)
                          (let ((new (make-hash-table)))
                            (hash-set! idx-eav entity new)
                            new)))
            (a-bucket (or (hash-ref e-bucket attr #f)
                          (let ((new (make-hash-table)))
                            (hash-set! e-bucket attr new)
                            new))))
       (hash-set! a-bucket tuple #t))
     (delta-add! attr tuple)))

(define-syntax dl-record!
   (syntax-rules ()
     ((_ dl type (attr value) ...) (let ((id (dl-next-id! dl)))
       (dl-assert! dl id (list type attr) value) ... id))))

; ----------------------------------------------------------------------
; Candidate enumeration + the join.
;
; Cases by ground-ness of the walked entity/attr (deep: no unbound pvar
; anywhere, so a partially-instantiated compound attr like (region ?name)
; routes to a scan, not an exact lookup):
;   (entity ground,     attr ground)     -> idx-eav   : smallest candidate set
;   (entity ground,     attr non-ground) -> idx-entity: scan the entity's tuples
;   (entity non-ground, attr ground)     -> idx-attr
;   (entity non-ground, attr non-ground) -> scan everything (via idx-entity;
;       every tuple appears exactly once under its entity). Costliest plan —
;       lead with a ground attr when you can.
; ----------------------------------------------------------------------

; Call TRY on every candidate tuple for walked condition CND.
(define (for-each-candidate dl cnd try)
  (define (scan bucket) (if bucket (hash-for-each (lambda (t _) (try t)) bucket)))
  (let ((e (car cnd)) (a (cadr cnd)))
    (cond
      ((and (pat-ground? e) (pat-ground? a))
       (let ((eb (hash-ref (datalog-idx-eav dl) e #f)))
         (scan (and eb (hash-ref eb a #f)))))
      ((pat-ground? e)
       (scan (hash-ref (datalog-idx-entity dl) e #f)))
      ((pat-ground? a)
       (scan (hash-ref (datalog-idx-attr dl) a #f)))
      (else
       (hash-for-each (lambda (_ bucket) (scan bucket))
                      (datalog-idx-entity dl))))))

; Same, but candidates come from the DELTA (attr -> tuples) instead of
; the full database. The delta is small, so the non-ground-attr case
; just scans all of it.
(define (for-each-delta-candidate delta cnd try)
  (let ((a (cadr cnd)))
    (if (pat-ground? a)
        (for-each try (hash-ref delta a '()))
        (hash-for-each (lambda (_ tuples) (for-each try tuples)) delta))))

; Depth-first nested-loop join. CONDS: list of condition patterns.
; EMIT: thunk called once per complete match, with ENV fully bound.
; When DELTA-POS is an integer, condition #DELTA-POS draws its candidates
; from DELTA instead of the full database (the semi-naive restriction).
(define (run-join dl conds env emit delta-pos delta)
  (let loop ((cs conds) (i 0))
    (if (null? cs)
        (emit)
        (let ((cnd (pat-walk (car cs) env)))
          (define (try tuple)
            (let ((trail (match-into! cnd tuple env)))
              (when trail
                (loop (cdr cs) (+ i 1))
                (undo-trail! env trail))))
          (if (and delta-pos (= i delta-pos))
              (for-each-delta-candidate delta cnd try)
              (for-each-candidate dl cnd try))))))

; ----------------------------------------------------------------------
; Rules.
; ----------------------------------------------------------------------

; id       - a fresh symbol naming the rule. Used as the EAV entity for
;            the rule's introspection facts ((?id (rule source) ...),
;            per-frame stats) and as the stats key. Scripts treat it as
;            an opaque handle found via (?page rules ?id).
; patterns - compiled condition triples (with <pvar>s)
; nvars    - environment size
; produce  - (lambda (env) tuple): builds the derived tuple per match
; attrs    - list of attr datums the conditions match on, or 'any when an
;            attr contains a variable (can't be predicted -> always eligible)
; source   - the source datum of the rule, for printing/introspection
(define-record-type <dl-rule>
  (make-dl-rule id patterns nvars produce attrs source)
  dl-rule?
  (id       dl-rule-id)
  (patterns dl-rule-patterns)
  (nvars    dl-rule-nvars)
  (produce  dl-rule-produce)
  (attrs    dl-rule-attrs)
  (source   dl-rule-source))

(define (dl-assert-rule! dl rule)
  (hash-set! (datalog-rdb dl) rule #t))

(define (dl-retract-rule! dl rule)
  (hash-remove! (datalog-rdb dl) rule))

; ----------------------------------------------------------------------
; Fixpoint statistics.
;
; Cheap counters recorded while the fixpoint runs; engine/scene.scm
; publishes them as engine-claimed facts at the START of the next frame
; (never mid-fixpoint — a stats fact appearing during the monotone run
; would trigger rules and change the very numbers being measured). Stats
; are therefore one frame stale, the same contract as Collect.
;
; Per rule: matches  = tuples produced by the join (under semi-naive
;                      evaluation this can count a derivation once per
;                      delta position — read it as relative load, and
;                      note that duplicates are deduped before bodies run)
;           fired    = body invocations (new code tuples; exact)
;           time-ms  = wall time spent in the rule's joins
;
; Thread-safety: cells are created serially (at the dl-fixpoint! reset
; and in the serial body-run hook); the par-mapped dl-apply-rule only
; writes to its own rule's pre-existing cell. A rule registered
; mid-fixpoint (e.g. by a save) has no cell yet and simply isn't counted
; until the next frame.
; ----------------------------------------------------------------------

(define *fixpoint-iterations* 0)
(define *fixpoint-new-facts* 0)
(define *rule-stats* (make-hash-table))   ; rule-id -> #(matches fired time-ms)

(define (dl-rule-stats id)
  (or (hash-ref *rule-stats* id #f) (vector 0 0 0.0)))

(define (dl-drop-rule-stats! id)
  (hash-remove! *rule-stats* id))

; Called (serially) by the body-run hook in engine/fixpoint.scm.
(define (dl-note-fired! id)
  (let ((cell (or (hash-ref *rule-stats* id #f)
                  (let ((v (vector 0 0 0.0)))
                    (hash-set! *rule-stats* id v)
                    v))))
    (vector-set! cell 1 (+ 1 (vector-ref cell 1)))))

(define (current-ms)
  (let ((t (gettimeofday)))
    (+ (* 1000.0 (car t)) (/ (cdr t) 1000.0))))

; All derived tuples for RULE. With PREV-DELTA (attr -> tuples of the
; previous iteration): the semi-naive union over delta positions — one
; join per condition, that condition restricted to the delta. Duplicates
; across positions are deduped downstream by the fixpoint's fact set.
; Without PREV-DELTA (#f): one full join (first iteration).
(define (dl-apply-rule dl rule prev-delta)
  (let* ((patterns (dl-rule-patterns rule))
         (produce  (dl-rule-produce rule))
         (nvars    (dl-rule-nvars rule))
         (cell     (hash-ref *rule-stats* (dl-rule-id rule) #f))
         (t0       (if cell (current-ms) 0))
         (out      '()))
    (define (collect delta-pos)
      (let ((env (make-env nvars)))
        (run-join dl patterns env
                  (lambda () (set! out (cons (produce env) out)))
                  delta-pos prev-delta)))
    (if prev-delta
        (let loop ((i 0) (cs patterns))
          (unless (null? cs)
            (collect i)
            (loop (+ i 1) (cdr cs))))
        (collect #f))
    (when cell
      (vector-set! cell 0 (+ (vector-ref cell 0) (length out)))
      (vector-set! cell 2 (+ (vector-ref cell 2) (- (current-ms) t0))))
    out))

; Eligibility: skip rules none of whose attrs saw a new fact last
; iteration. 'any rules (variable in attr position) always run.
(define (rules-to-evaluate dl prev-delta)
  (if (not prev-delta)
      (hashtable-keys (datalog-rdb dl))
      (let ((eligible '()))
        (hash-for-each
          (lambda (rule _)
            (let ((attrs (dl-rule-attrs rule)))
              (when (or (eq? attrs 'any)
                        (any (lambda (a) (hash-ref prev-delta a #f)) attrs))
                (set! eligible (cons rule eligible)))))
          (datalog-rdb dl))
        eligible)))

; ----------------------------------------------------------------------
; The fixpoint.
;
; Called with the list of newly derived tuples each iteration, after they
; are indexed. engine/fixpoint.scm sets this to run When bodies; the
; default keeps this file self-contained for pure-datalog use.
; ----------------------------------------------------------------------

(define *fixpoint-new-facts-hook* (lambda (dl new) #f))

(define (dl-fixpoint! dl)
  (for-each (lambda (fact) (dl-retract! dl fact)) (hashtable-keys (datalog-idb dl)))
  (set-datalog-idb! dl (make-hash-table))
  ; reset stats: fresh cells for every registered rule (serial — this is
  ; also what makes the par-mapped writes in dl-apply-rule safe)
  (set! *fixpoint-iterations* 0)
  (set! *fixpoint-new-facts* 0)
  (hash-for-each
    (lambda (rule _) (hash-set! *rule-stats* (dl-rule-id rule) (vector 0 0 0.0)))
    (datalog-rdb dl))
  (reset-delta!)
  (dl-fixpoint-iterate dl #f))

(define (dl-fixpoint-iterate dl prev-delta)
  (let ((rules (rules-to-evaluate dl prev-delta)))
    (if (null? rules) #t
        (begin
          (reset-delta!)   ; this iteration's delta accumulates here
          (let* ((facts (par-map (lambda (rule) (dl-apply-rule dl rule prev-delta)) rules))
                 (factset (fold (lambda (fs acc) (set-extend! acc fs)) (make-hash-table) facts))
                 (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
            (set! *fixpoint-iterations* (+ 1 *fixpoint-iterations*))
            (set! *fixpoint-new-facts* (+ *fixpoint-new-facts* (length new)))
            (set-extend! (datalog-idb dl) new)
            (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
            (*fixpoint-new-facts-hook* dl new)
            (if (not (null? new)) (dl-fixpoint-iterate dl *delta*)))))))

; todo: remove from edb? doesn't matter atm
(define (dl-retract! dl tuple)
   (let ((entity (car tuple))
         (attr (cadr tuple))
         (idx-entity (datalog-idx-entity dl))
         (idx-attr (datalog-idx-attr dl))
         (idx-eav (datalog-idx-eav dl)))
     (let ((m (hash-ref idx-entity entity #f)))
       (if m (hash-remove! m tuple)))
     (let ((m (hash-ref idx-attr attr #f)))
       (if m (hash-remove! m tuple)))
     (let* ((e-bucket (hash-ref idx-eav entity #f))
            (a-bucket (and e-bucket (hash-ref e-bucket attr #f))))
       (if a-bucket (hash-remove! a-bucket tuple)))))

; ----------------------------------------------------------------------
; Expansion-time helpers shared by dl-query / dl-rule! here and When in
; engine/dsl.scm. eval-when makes them available to the macro
; transformers below (and in later-loaded files) regardless of how the
; tree is loaded.
; ----------------------------------------------------------------------

(eval-when (expand load eval)
  ; A logic variable: any symbol whose first char is #\?, e.g. ?p, ?color.
  (define (logic-var-sym? x)
    (and (symbol? x)
         (let ((s (symbol->string x)))
           (and (positive? (string-length s))
                (char=? (string-ref s 0) #\?)))))

  (define (syntax-logic-var? stx)
    (and (identifier? stx) (logic-var-sym? (syntax->datum stx))))

  ; #t when a logic var appears ANYWHERE in DATUM, including nested inside
  ; a compound attr such as (region ?name). Used to decide rule
  ; eligibility: such an attr can't be predicted at expand time, so the
  ; rule is marked 'any (always-eligible).
  (define (contains-logic-var? datum)
    (cond ((logic-var-sym? datum) #t)
          ((pair? datum) (or (contains-logic-var? (car datum))
                             (contains-logic-var? (cdr datum))))
          (else #f)))

  ; All distinct logic-var identifiers in TREE, reverse first-seen order
  ; (callers reverse). Skips (quote ...) subtrees, and (dl-query ...)
  ; subtrees — a query inside a When body binds its own vars, so they
  ; must not look unbound to When's body check. Descends into unquote
  ; so ,?x-style vars are found too. bound-identifier=? dedup keeps a
  ; macro-introduced ?x distinct from a user-written ?x (they get
  ; separate slots even though they spell the same symbol).
  (define (collect-pattern-vars tree acc)
    (syntax-case tree (quote dl-query)
      ((quote _) acc)
      ((dl-query . _) acc)
      ((a . d) (collect-pattern-vars #'d (collect-pattern-vars #'a acc)))
      (id (syntax-logic-var? #'id)
          (if (find (lambda (v) (bound-identifier=? v #'id)) acc)
              acc
              (cons #'id acc)))
      (_ acc)))

  (define (pattern-var-slot id vars)
    (list-index (lambda (v) (bound-identifier=? v id)) vars))

  ; Build the quasiquote TEMPLATE (to be wrapped in a quasiquote by the
  ; caller) that constructs the runtime pattern for TREE:
  ;   ?var          -> ,(make-pvar <slot> '?var)
  ;   ,expr         -> ,expr    (runtime splice; ,?var means the var)
  ;   this          -> ,this    (the page id, via the dsl's syntax
  ;                              parameter — flat-namespace datum compare)
  ;   anything else -> literal
  (define (pattern-template tree vars)
    (define (pvar-for id)
      #`(unquote (make-pvar #,(pattern-var-slot id vars) '#,id)))
    (let walk ((t tree))
      (syntax-case t (unquote)
        ((unquote e) (syntax-logic-var? #'e) (pvar-for #'e))
        ((unquote e) t)
        ((a . d) (cons (walk #'a) (walk #'d)))
        (id (syntax-logic-var? #'id) (pvar-for #'id))
        (id (and (identifier? #'id) (eq? (syntax->datum #'id) 'this))
            #`(unquote #,t))
        (other #'other)))))

; ----------------------------------------------------------------------
; (dl-query dl ((e attr v) ...) result-expr)
;
; Run a conjunctive query and collect RESULT-EXPR once per match, with
; the ?vars of the conditions in scope. Result order is unspecified.
; ----------------------------------------------------------------------
(define-syntax dl-query
  (lambda (stx)
    (syntax-case stx ()
      ((_ dl ((cx cnd cy) ...) result)
       (let* ((vars (reverse (collect-pattern-vars #'((cx cnd cy) ...) '())))
              (tpl  (pattern-template #'((cx cnd cy) ...) vars))
              (n    (length vars)))
         #`(let ((env (make-env #,n))
                 (out '()))
             (run-join dl #,(list #'quasiquote tpl) env
                       (lambda ()
                         (let #,(map (lambda (v i) #`(#,v (vector-ref env #,i)))
                                     vars (iota n))
                           (set! out (cons result out))))
                       #f #f)
             out))))))

; ----------------------------------------------------------------------
; (dl-rule! dl (attr e v) :- (attr e v) ...)
;
; Assert a pure rule: derive the head tuple for every match of the body.
; Head variables must appear in the body (range restriction) — checked
; at expand time.
; ----------------------------------------------------------------------
(define-syntax dl-rule!
  (lambda (stx)
    (syntax-case stx (:-)
      ((_ dl (ha he hv) :- (ba be bv) ...)
       (let* ((head      #'(he ha hv))
              (body      #'((be ba bv) ...))
              (vars      (reverse (collect-pattern-vars body '())))
              (head-vars (reverse (collect-pattern-vars head '())))
              (n         (length vars)))
         (for-each
           (lambda (hv*)
             (unless (find (lambda (v) (bound-identifier=? v hv*)) vars)
               (syntax-violation 'dl-rule!
                 "head variable does not appear in rule body" stx hv*)))
           head-vars)
         (let* ((body-tpl    (pattern-template body vars))
                (head-tpl    (pattern-template head vars))
                (attr-datums (syntax->datum #'(ba ...)))
                (attrs       (if (any contains-logic-var? attr-datums)
                                 'any attr-datums)))
           #`(dl-assert-rule! dl
               (make-dl-rule
                 (gensym "rule-")
                 #,(list #'quasiquote body-tpl)
                 #,n
                 (let ((head #,(list #'quasiquote head-tpl)))
                   (lambda (env) (pat-walk head env)))
                 '#,(datum->syntax stx attrs)
                 '#,(datum->syntax stx
                      (syntax->datum #'((ha he hv) :- (ba be bv) ...)))))))))))

#| HELPER FUNCTIONS |#
(define (set-extend! m keys)
  (for-each (lambda (key)
    (hash-set! m key #t))
  keys) m)

(define (set-difference a b)
  (let ((m (make-hash-table)))
    (for-each (lambda (key)
      (if (not (hash-ref b key #f)) (hash-set! m key #t)))
    (hashtable-keys a)) m))

; hash-table-keys not available?
(define (hashtable-keys ht)
  (hash-fold (lambda (k v acc) (cons k acc)) '() ht))
