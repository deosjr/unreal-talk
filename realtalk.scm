(include "datalog.scm")
(use-modules (ice-9 threads))

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)

; Note on derived Claim/Wish inside When: the When macro auto-rewrites
; (Claim id attr v) and (Wish id attr v) inside its body to internal
; helpers `derived-claim!` / `derived-wish!`, which write only to the
; IDB. Those facts are re-derived each fixpoint and retracted by the
; next dl-fixpoint! reset. Nested When forms are rejected at expand
; time — the lifecycle of a rule that produces rules is not designed.

; Remember/Forget can store state beyond a page's lifetime.
; For now we will use an in-memory db so state is still scoped to RealTalkOS lifetime
; https://dynamicland.org/archive/2020/Memories
; "The remembered statements on an object are only there when the object itself is there and running"
; Values are unique per key, for now. Replacing is automatic.
; Page-local vars are still a thing too, they are just less reliable since detection isn't 100% stable

(define *procs* (make-hash-table))
(define *rule-procs* (make-hash-table))
; ids we've already tried to load. Tried-once-and-failed entries stay in
; the set so we don't re-read a missing file every fixpoint when a tag is
; on the table — the has-error fact does the messaging.
(define *load-attempted* (make-hash-table))

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
  (filter (lambda (id) (not (hash-ref *pages-in-scene-prev* id #f)))
    (hashtable-keys *pages-in-scene*)))

(define (get-removed-pages)
  (filter (lambda (id) (not (hash-ref *pages-in-scene* id #f)))
    (hashtable-keys *pages-in-scene-prev*)))

(define (get-stable-pages)
  (filter (lambda (id) (hash-ref *pages-in-scene* id #f))
    (hashtable-keys *pages-in-scene-prev*)))

(define (receive-key-down key)
  (assert-key key))
(define (receive-time-detect d)
  (assert-time-detect d))
(define (receive-time-scm d)
  (assert-time-scm d))

(define (receive-pages-found pages)
  (fill-image projection 0 0 0) ;; fill black
  (for-each (lambda (page)
    (let ((id (car page))
          (points (cadr page))
          (rotation (caddr page)))
      (update-global-page-registry id)
      (update-page-geometry id points rotation))) pages)
  (for-each (lambda (id) (begin
       (page-moved-from-table id)
       (forget-all id))) (get-removed-pages))
  (for-each (lambda (id) (begin
       (ensure-loaded! id)
       (page-moved-onto-table id)
       (remember-all id))) (get-new-pages))
  (update-memories)
  (set! *pages-in-scene-prev* *pages-in-scene*)
  (set! *pages-in-scene* (make-hash-table))
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

; ----------------------------------------------------------------------
; (When (cond ...) do body ...)
;
; A rule with side effects. Compiles each use site to two cooperating
; pieces wired together by a unique symbol:
;
;   1. A `rule` value: a minikanren goal that unifies the result `q` with
;      a tuple `(this 'code (code-name . args))`. When dl-fixpoint-iterate
;      runs the rules, satisfying the conditions produces one such tuple
;      per match, with `args` bound to the logic-var values.
;
;   2. A `code` procedure: the rule's body wrapped as
;      (lambda (this ?var1 ?var2 ...) body...). Stored in *rule-procs*
;      under code-name. After dl-fixpoint-iterate accumulates new facts
;      it walks them, looks each proc up by code-name, and applies it
;      to the bound args. That's where the body's side effects run.
;
; So the When macro never executes the body directly — it just packages
; the body for later application during fixpoint iteration.
;
; Condition surface syntax: a small DSL where
;   ?var          -> a logic variable (matched/bound by minikanren)
;   this          -> the page-id Scheme binding (runtime value)
;   anything else -> a literal datum (used as the datalog attr or as
;                    a literal structural component of entity/value)
; No commas. The macro inserts the unquotes that dl-findo's internal
; quasiquote needs. The body, in contrast, is plain Scheme — logic
; vars there are just lexical bindings introduced by the rule lambda.
;
; A few subtleties:
;
; * Logic variables (`?x`, `?p`, ...) are not legal Scheme identifiers,
;   so we can't use syntax-case patterns to bind them. We work on the
;   datum tree (via syntax->datum), substitute each ?var with a fresh
;   gensym, then datum->syntax everything back. The gensym replacement
;   gives us per-rule hygiene: two When forms that both use ?x get
;   distinct identifiers, no accidental capture.
;
; * Re-anchoring free identifiers: when we datum->syntax a symbol like a
;   user-defined helper procedure, we pass `stx` (the macro input) as
;   the context so the symbol resolves at the *user's* source location,
;   not in this realtalk module. Without that, calls to user helpers
;   from inside the body wouldn't resolve.
;
; * `this` is anchored once via with-syntax so the rule-lambda's `this`
;   parameter, the body's `this` references, and the (Claim/Wish ...)
;   -> (derived-claim!/derived-wish! this ...) rewrites all refer to
;   the same identifier — and so the lambda binding captures every body
;   reference.
;
; Two walkers do the work:
;
;   xform-cond (conditions):
;     ?var          -> (unquote <its-gensym>)
;     this          -> (unquote this)
;     other symbol  -> re-anchored to user's stx (literal)
;     pair          -> recurse on car and cdr
;
;   xform (body):
;     ?var          -> its assigned gensym (bare — it's a lexical ref)
;     (Claim ...)   -> (derived-claim! this ...)   ; lives one fixpoint
;     (Wish  ...)   -> (derived-wish!  this ...)
;     (When  ...)   -> compile-time error (nested rules' lifecycle is
;                      not designed)
;     other symbol  -> re-anchored to user's stx
;     pair          -> recurse on car and cdr
; ----------------------------------------------------------------------
(define-syntax When
  (lambda (stx)
    ; A logic variable: any symbol whose first char is #\?, e.g. ?p, ?color.
    (define (logic-var? x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (positive? (string-length s))
                  (char=? (string-ref s 0) #\?)))))

    ; All distinct logic variables in DATUM, in first-seen order. The
    ; order matters: it fixes the parameter order of both the body-
    ; procedure and the rule's lambda, so the args produced by
    ; minikanren match up.
    (define (collect-logic-vars datum)
      (let walk ((d datum) (seen '()))
        (cond ((logic-var? d) (if (member d seen) seen (cons d seen)))
              ((pair? d) (walk (cdr d) (walk (car d) seen)))
              (else seen))))

    ; Transform DATUM for splicing back into the emitted code:
    ;   - SYM->GEN maps each ?var to its chosen gensym (a syntax object).
    ;   - All other symbols become syntax objects anchored to stx (the
    ;     macro input), so they resolve at the user's source location.
    (define (xform datum sym->gen)
      (define (recur d) (xform d sym->gen))
      (define (here sym) (datum->syntax stx sym))
      (cond
        ((logic-var? datum)
         (cond ((assq datum sym->gen) => cdr)
               (else (here datum))))
        ((not (pair? datum)) (here datum))
        ((eq? (car datum) 'Claim)
         (cons (here 'derived-claim!) (cons (here 'this) (map recur (cdr datum)))))
        ((eq? (car datum) 'Wish)
         (cons (here 'derived-wish!) (cons (here 'this) (map recur (cdr datum)))))
        ((eq? (car datum) 'When)
         (syntax-violation 'When "nested When is not supported" stx))
        (else (cons (recur (car datum)) (recur (cdr datum))))))

    ; Like xform but for condition triples. Conditions are eventually
    ; quasi-quoted by dl-findo, so any value we want pulled from scope
    ; (logic vars and `this`) needs to be wrapped in (unquote ...) here.
    ; Bare symbols stay literal — they're the datalog attr keys and the
    ; literal structural skeleton around bound positions.
    (define (xform-cond datum sym->gen)
      (define (recur d) (xform-cond d sym->gen))
      (define (here sym) (datum->syntax stx sym))
      (cond
        ((logic-var? datum)
         (let ((p (assq datum sym->gen)))
           (if p
               ; build (unquote <gen>) as a 2-element list
               (list (here 'unquote) (cdr p))
               (here datum))))
        ((eq? datum 'this)
         (list (here 'unquote) (here 'this)))
        ((not (pair? datum)) (here datum))
        (else (cons (recur (car datum)) (recur (cdr datum))))))

    (syntax-case stx (do)
      ((_ ((cx condition cy) ...) do statement ...)
       (with-syntax ((this (datum->syntax stx 'this)))
         (let* ((conds (syntax->datum #'((cx condition cy) ...)))
                (body  (syntax->datum #'(statement ...)))
                ; Walk conds *and* body together so a ?var used only in
                ; the body (e.g. shadowing a condition var) still gets a
                ; gensym slot; the rule-lambda passes it through unbound,
                ; minikanren handles it.
                (vars  (collect-logic-vars (cons conds body)))
                (gens  (generate-temporaries vars))
                (sym->gen (map cons vars gens))
                (conds* (xform-cond conds sym->gen))
                (body*  (xform body  sym->gen))
                ; Semi-naive: extract the attr from each cond (the middle
                ; element of the user's (cx condition cy) triple). If any
                ; is a logic-var we can't predict at expand time which
                ; attrs the rule depends on — mark as 'any so the rule is
                ; always eligible.
                (body-attrs (map cadr conds))
                (any-var? (let loop ((as body-attrs))
                            (cond ((null? as) #f)
                                  ((logic-var? (car as)) #t)
                                  (else (loop (cdr as))))))
                (rule-attrs-datum (if any-var? 'any body-attrs))
                ; Wrap the literal in syntax anchored at stx so the
                ; template's #, splice gets a proper syntax object, not
                ; a raw datum (which the expander would reject as
                ; "raw symbol in macro output").
                (rule-attrs (datum->syntax stx rule-attrs-datum)))
           #`(let* ((code (lambda (this #,@gens) #,@body*))
                    (code-name (gensym))
                    (rule (fresh-vars #,(+ 1 (length vars))
                            (lambda (q #,@gens)
                              (conj (equalo q (list this 'code
                                                    (cons code-name (list #,@gens))))
                                    (dl-findo (get-dl) #,conds*))))))
               (hash-set! *rule-procs* code-name code)
               (dl-assert! (get-dl) this 'rules rule)
               (dl-assert-rule! (get-dl) rule '#,rule-attrs))))))))

; Used by the When macro to implement Claim/Wish *inside* a rule body.
; Mirror of Claim/Wish but routed through dl-assert-derived! so writes
; land in the IDB (re-derived next iteration if the rule still fires,
; retracted by the next dl-fixpoint! reset when it stops).
;
; Not part of the public API — user code should just write (Claim ...) /
; (Wish ...) inside a When body and let the macro do the routing.
(define (derived-claim! this id attr value)
  (dl-assert-derived! (get-dl) this 'claims (list id attr value))
  (dl-assert-derived! (get-dl) id attr value))

(define (derived-wish! this id attr value)
  (dl-assert-derived! (get-dl) this 'wishes (list id attr value)))

; redefine dl-fixpoint! injecting code execution as result of rules.
; Semi-naive: only re-evaluate rules whose body attrs intersect with
; the deltas of the previous iteration. See datalog.scm for the
; bookkeeping (*delta-attrs*, rules-to-evaluate).
(define (dl-fixpoint! dl)
  (for-each (lambda (fact) (dl-retract! dl fact)) (hashtable-keys (datalog-idb dl)))
  (set-datalog-idb! dl (make-hash-table))
  (reset-delta-attrs!)
  (dl-fixpoint-iterate dl #f))

(define (dl-fixpoint-iterate dl prev-delta)
  (let ((rules (rules-to-evaluate dl prev-delta)))
    (if (null? rules) #t
        (begin
          (reset-delta-attrs!)
          (let* ((facts (par-map (lambda (rule) (dl-apply-rule dl rule)) rules))
                 (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hash-table)))
                 (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
            (set-extend! (datalog-idb dl) new)
            (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
            ; result of dl_apply_rule should be a tuple (this 'code (proc . args))
            ; Each body invocation is wrapped: an exception in one rule
            ; body must not abort the fixpoint or escape into Go. The
            ; error is logged and asserted as a derived fact so other
            ; pages (e.g. an error-display page) can react.
            (for-each (lambda (c)
              (let ((this (car c))
                    (proc (caaddr c))
                    (args (cdaddr c)))
                (catch #t
                  (lambda () (apply (hash-ref *rule-procs* proc #f) this args))
                  (lambda (key . eargs)
                    (format (current-error-port)
                            "rule body error in page ~a: ~s ~s~%" this key eargs)
                    (dl-assert-derived! dl this 'has-error
                      (format #f "rule: ~a: ~s" key eargs)))))) new)
            (if (not (null? new)) (dl-fixpoint-iterate dl *delta-attrs*)))))))

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

; Clear any 'has-error facts attached to PID. Used both on successful
; recompile (to drop a stale error sticking around from a previous bad
; save) and when a page leaves the table (cleanup).
(define (dl-retract-page-errors! pid)
  (let ((errs (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid has-error ,x) )))))))
    (for-each (lambda (msg) (dl-retract! dl `(,pid has-error ,msg))) errs)))

; Retract any existing (pid (page code) _) facts. Used by load-page and
; save-page before asserting a new value, so the (page code) attribute
; behaves like a single-valued slot rather than accumulating every
; version the page has ever had.
(define (dl-retract-page-code! pid)
  (let ((strs (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page code) ,x) )))))))
    (for-each (lambda (str) (dl-retract! dl `(,pid (page code) ,str))) strs)))

; only run page code when newly in bounds of table.
; Wrapped: page top-level code that throws (bad Claim args, undefined
; helpers, etc.) must not crash the system. Old proc stays in *procs*
; until next save; error fact is asserted so it can be displayed.
(define (page-moved-onto-table pid)
  (catch #t
    (lambda () (execute-page pid))
    (lambda (key . args)
      (format (current-error-port)
              "page init error in page ~a: ~s ~s~%" pid key args)
      (dl-retract-page-errors! pid)
      (dl-assert! dl pid 'has-error
        (format #f "init: ~a: ~s" key args)))))

; then retract all 'this claims x' and 'this rules x' from dl-db when newly out of table bounds
(define (page-moved-from-table pid)
  (retract-page-geometry pid)
  (dl-retract-page-errors! pid)
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

(define (assert-time-detect d)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time detect ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time detect ,claim))) claims))
  (dl-assert! dl 'time 'detect d))

(define (assert-time-scm d)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time scm ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time scm ,claim))) claims))
  (dl-assert! dl 'time 'scm d))

; if key == -1 then no key was pressed
(define (assert-key key)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (key down ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(key down ,claim))) claims))
  (if (not (= key -1)) (dl-assert! dl 'key 'down key)))

(define (read-page-code id)
  (call-with-input-file (format #f "scripts/~d.scm" id) (lambda (port)
    (get-string-all port)) #:encoding "utf-8"))

; Wrap user code in (make-page-code ...) and eval it, returning the
; resulting page procedure. On any error: log, assert a 'has-error fact
; on the page (visible to error-display pages), and return #f so the
; caller can decide what to do (load-page skips; save-page keeps the
; old proc in place).
(define (try-compile-page id phase code-str)
  (catch #t
    (lambda () (eval-string (format #f "(make-page-code ~a)" code-str)))
    (lambda (key . args)
      (format (current-error-port)
              "~a error in page ~a: ~s ~s~%" phase id key args)
      (dl-retract-page-errors! id)
      (dl-assert! dl id 'has-error
        (format #f "~a: ~a: ~s" phase key args))
      #f)))

; Empty source we assert as (page code) when the script file is missing
; so the editor (scripts/1.scm) — whose `When` matches `(?p (page code)
; ?str)` — can still see the tag and let the user write code from
; scratch. A save replaces the placeholder via dl-retract-page-code!.
(define empty-script "")

; Read the script file then compile. Wrapped: a missing scripts/<id>.scm
; (or an unreadable one) should not crash main.scm and should surface as
; a has-error fact so 9005 renders "ERR: no script for tag N" at the
; offending tag. Compile errors are already handled by try-compile-page.
;
; Records the attempt in *load-attempted* so ensure-loaded! won't keep
; re-trying a missing file every fixpoint.
(define (load-page id)
  (hash-set! *load-attempted* id #t)
  (dl-retract-page-code! id)
  (let ((str (catch #t
               (lambda () (read-page-code id))
               (lambda (key . args)
                 (format (current-error-port)
                         "could not read script for tag ~a: ~s ~s~%" id key args)
                 (dl-retract-page-errors! id)
                 (dl-assert! dl id 'has-error
                   (format #f "no script for tag ~a" id))
                 ; Assert an empty placeholder so the editor can pick up
                 ; the page despite the missing file.
                 (dl-assert! dl id '(page code) empty-script)
                 #f))))
    (if str
      (let ((proc (try-compile-page id 'load str)))
        (dl-assert! (get-dl) id '(page code) str)
        (if proc (hash-set! *procs* id proc))))))

; Lazy-load: try once per session per tag id. Subsequent placements of
; the same tag skip the file read (uses the cached *procs* entry, or
; respects a previous failed attempt). Called from receive-pages-found
; when a tag is first seen.
(define (ensure-loaded! id)
  (if (not (hash-ref *load-attempted* id #f))
    (load-page id)))

(define (save-page id code-str)
  (let ((proc (try-compile-page id 'save code-str)))
    ; If compile failed, leave *procs* and (page code) untouched so the
    ; page keeps running its last good version while the user fixes the
    ; new source — has-error was already asserted by try-compile-page.
    (if proc
      (begin
        (dl-retract-page-errors! id)
        (dl-retract-page-code! id)
        (dl-assert! (get-dl) id '(page code) code-str)
        (hash-set! *procs* id proc)
        (page-moved-from-table id)
        (page-moved-onto-table id)))))

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
(define *remember* (make-hash-table)) ; stages memories to remember

(define (Remember on id key value)
  (Forget on id key) ; always replace
  (hash-set! *remember* (list on id key) value))

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

(define (remember-all on)
  (let ((memories (memories-on on)))
    (for-each (lambda (mem)
      (let ((id (cadr mem))
            (key (caddr mem))
            (value (hash-ref *memories* mem #f)))
        (hash-set! *remember* (list on id key) value)))
      memories)))

(define (update-memories)
  (for-each (lambda (dbkey)
    (dl-retract! (get-dl) dbkey)) *forget*)
  (for-each (lambda (mem)
    (let ((on (car mem))
          (id (cadr mem))
          (key (caddr mem))
          (value (hash-ref *remember* mem #f)))
      (if (hash-ref *pages-in-scene* on #f) (begin
        (hash-set! *memories* mem value)
        (dl-assert! (get-dl) id key value)))))
      (hashtable-keys *remember*))
  (set! *forget* '())
  (set! *remember* (make-hash-table)))
