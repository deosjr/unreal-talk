; ----------------------------------------------------------------------
; The RealTalk DSL: the macros user/page code is written in.
;
;   make-page-code  - wrap a page's top-level code as (lambda (this) ...)
;   Claim / Wish    - assert facts / desires (persistent, EDB)
;   When            - a rule with side effects
;   Collect         - cross-frame map-reduce / negation over a query
;
; derived-claim! / derived-wish! are the IDB-routed helpers When rewrites
; Claim/Wish to inside a rule body. See datalog.scm for the engine these
; sit on top of, and fixpoint.scm for where the rule bodies actually run.
;
; Note on derived Claim/Wish inside When: the When macro auto-rewrites
; (Claim id attr v) and (Wish id attr v) inside its body to internal
; helpers `derived-claim!` / `derived-wish!`, which write only to the
; IDB. Those facts are re-derived each fixpoint and retracted by the
; next dl-fixpoint! reset. Nested When forms are rejected at expand
; time — the lifecycle of a rule that produces rules is not designed.
; ----------------------------------------------------------------------

; Maps a When's generated code-name (a gensym) to the procedure that runs
; its rule body. The When macro populates it at page-execution time;
; fixpoint.scm looks each proc up by name and applies it to the bound args.
(define *rule-procs* (make-hash-table))

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

    ; #t when a logic var appears ANYWHERE in DATUM, including nested inside
    ; a compound attr such as (region ?name). Used for semi-naive eligibility:
    ; a rule whose attr contains a logic var can't be predicted at expand
    ; time, so it must be marked 'any (always-eligible).
    (define (contains-logic-var? datum)
      (cond ((logic-var? datum) #t)
            ((pair? datum) (or (contains-logic-var? (car datum))
                               (contains-logic-var? (cdr datum))))
            (else #f)))

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
                                  ((contains-logic-var? (car as)) #t)
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

; ----------------------------------------------------------------------
; (Collect (cond ...) emit emit-expr as name do body ...)
;
; Cross-frame map-reduce / negation over a datalog query. The engine's
; fixpoint is monotone with no script-visible "converged" signal, so you
; cannot aggregate within a frame. Collect sidesteps that with a per-use
; double buffer swapped once per frame on the (time now) tick:
;
;   * MAP    - an accumulator When over (cond ...). For every matching
;              tuple it conses emit-expr onto the current buffer. emit-expr
;              is plain Scheme over the query's ?vars (so it can compute,
;              e.g. a distance, not just project).
;   * SHUFFLE- the frame boundary. When (time now) advances we swap: the
;              just-completed buffer (a fully converged set) becomes `prev`,
;              the current buffer resets. The swap is idempotent on the
;              timestamp, so it happens exactly once per frame regardless of
;              which/how many rules touch the buffers, in any firing order.
;   * REDUCE - a time-keyed consumer When that fires once per frame *even
;              when nothing matched* (this is what makes negation/forall
;              expressible). It binds `name` to the COMPLETE set of emits
;              from the PREVIOUS frame and runs body. Claim/Wish in body
;              are rewritten by When as usual.
;
; Consequence: results lag one frame. Chaining Collects stacks the lag.
;
; Implementation note: each Collect needs private buffer state shared
; between its generated `define`s and its two `When` bodies. We cannot use
; let-bound (hygienic) temporaries because When round-trips its body
; through syntax->datum and re-anchors every identifier to the call site,
; which flattens hygiene. So we mint interned-but-unique top-level symbols
; (gensym printname) that resolve by global name from anywhere, and a
; unique ?-prefixed logic var for the injected (time now) clock.
(define-syntax Collect
  (lambda (stx)
    (syntax-case stx (emit as do)
      ((_ (cnd ...) emit e as name do body ...)
       ; Buffer names must be interned so they resolve as top-level bindings
       ; by global name (When flattens its body through syntax->datum and
       ; re-anchors). Guile's gensym already returns interned symbols.
       (let* ((cur   (gensym "collect-cur-"))
              (prv   (gensym "collect-prev-"))
              (tt    (gensym "collect-t-"))
              (swp   (gensym "collect-swap-"))
              ; a logic var (?-prefixed) for the per-frame clock; unique so
              ; it never collides with a ?var in the user's query/body.
              (clk   (string->symbol
                       (string-append "?" (symbol->string (gensym "collect-clk-")))))
              (conds (syntax->datum #'(cnd ...)))
              (emitd (syntax->datum #'e))
              (named (syntax->datum #'name))
              (bodyd (syntax->datum #'(body ...)))
              ; MAP rule: user conds + injected clock; swap then accumulate.
              (acc-conds (append conds (list (list 'time 'now clk))))
              (acc-body  (list (list swp clk)
                               (list 'set! cur (list 'cons emitd cur))))
              ; REDUCE rule: clock only; swap, bind name=prev, run body.
              (con-conds (list (list 'time 'now clk)))
              (con-body  (list (list swp clk)
                               (cons 'let
                                     (cons (list (list named prv)) bodyd))))
              (form
                `(begin
                   (define ,cur (quote ()))
                   (define ,prv (quote ()))
                   (define ,tt 0)
                   (define (,swp t)
                     (if (not (equal? t ,tt))
                         (begin (set! ,prv ,cur)
                                (set! ,cur (quote ()))
                                (set! ,tt t))))
                   (When ,acc-conds do ,@acc-body)
                   (When ,con-conds do ,@con-body))))
         (datum->syntax stx form))))))
