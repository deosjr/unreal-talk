; ----------------------------------------------------------------------
; The RealTalk DSL: the macros user/page code is written in.
;
;   make-page-code  - wrap a page's top-level code as (lambda (self) ...)
;   this            - syntax parameter: the current page id inside page code
;   Claim / Wish    - assert facts / desires (persistent, EDB)
;   When            - a rule with side effects
;   Collect         - cross-frame map-reduce / negation over a query
;
; this / Claim / Wish / When are syntax parameters: their top-level
; meaning is the EDB-asserting one, and When re-binds them with
; syntax-parameterize inside its body so that
;   this          -> the entity the rule fired on
;   Claim / Wish  -> derived-claim! / derived-wish!, which write only to
;                    the IDB. Those facts are re-derived each fixpoint
;                    and retracted by the next dl-fixpoint! reset.
;   When          -> expand-time error (the lifecycle of a rule that
;                    produces rules is not designed)
; Unlike a body-walking rewrite, the parameterization covers exactly the
; body's lexical region — including uses introduced by other macros —
; and nothing else. See datalog.scm for the engine these sit on top of,
; and fixpoint.scm for where the rule bodies actually run.
; ----------------------------------------------------------------------

; Maps a When's generated code-name (a gensym) to the procedure that runs
; its rule body. The When macro populates it at page-execution time;
; fixpoint.scm looks each proc up by name and applies it to the bound args.
(define *rule-procs* (make-hash-table))

; The page id. Outside page code it is an expand-time error; make-page-code
; binds it to the page the code runs as, and When re-binds it inside rule
; bodies to the entity the rule fired on (the same page id in practice).
(define-syntax-parameter this
  (lambda (stx)
    (syntax-violation 'this "'this' used outside of page code" stx)))

(define-syntax make-page-code
  (syntax-rules ()
    ((_ body ...)
     (lambda (self)
       (syntax-parameterize ((this (identifier-syntax self)))
         body ...)))))

(define-syntax-parameter Claim
  (syntax-rules ()
    ((_ id attr value)
     (begin
       (dl-assert! (get-dl) this 'claims (list id attr value))
       (dl-assert! (get-dl) id attr value)))))

(define-syntax-parameter Wish
  (syntax-rules ()
    ((_ id attr value)
     (dl-assert! (get-dl) this 'wishes (list id attr value)))))

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
;      (lambda (self ?var1 ?var2 ...) body...). Stored in *rule-procs*
;      under code-name. After dl-fixpoint-iterate accumulates new facts
;      it walks them, looks each proc up by code-name, and applies it
;      to the bound args. That's where the body's side effects run.
;
; So the When macro never executes the body directly — it just packages
; the body for later application during fixpoint iteration.
;
; Condition surface syntax: a small DSL where
;   ?var          -> a logic variable (matched/bound by minikanren)
;   this          -> the page id (the syntax parameter above)
;   anything else -> a literal datum (used as the datalog attr or as
;                    a literal structural component of entity/value)
; No commas. The macro inserts the unquotes that dl-findo's internal
; quasiquote needs. The body, in contrast, is plain Scheme — logic
; vars there are just lexical bindings introduced by the rule lambda.
;
; Implementation: logic vars like ?x ARE ordinary Scheme identifiers
; (#\? is a legal initial character), so no datum round-trip is needed.
; We walk the conditions and body as SYNTAX OBJECTS, collect the
; distinct ?-identifiers in first-seen order, and splice them verbatim
; as the parameters of both generated lambdas: condition occurrences are
; bound by the rule goal's lambda, body occurrences by the code lambda.
; Deduping with bound-identifier=? keeps a macro-introduced ?x distinct
; from a user-written ?x. The body passes through untouched — full
; hygiene — which is what lets Collect below be a plain syntax-rules
; macro. Vars are collected from conds AND body together so a ?var used
; only in the body still gets a slot; the rule passes it through
; unbound, minikanren reifies it.
; ----------------------------------------------------------------------
(define-syntax-parameter When
  (lambda (stx)
    ; A logic variable: any symbol whose first char is #\?, e.g. ?p, ?color.
    (define (logic-var-sym? x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (positive? (string-length s))
                  (char=? (string-ref s 0) #\?)))))
    (define (logic-var? id) (logic-var-sym? (syntax->datum id)))

    ; #t when a logic var appears ANYWHERE in DATUM, including nested inside
    ; a compound attr such as (region ?name). Used for semi-naive eligibility:
    ; a rule whose attr contains a logic var can't be predicted at expand
    ; time, so it must be marked 'any (always-eligible).
    (define (contains-logic-var? datum)
      (cond ((logic-var-sym? datum) #t)
            ((pair? datum) (or (contains-logic-var? (car datum))
                               (contains-logic-var? (cdr datum))))
            (else #f)))

    ; All distinct logic-var identifiers in TREE, reverse first-seen order
    ; (callers reverse). The order matters: it fixes the parameter order of
    ; both the body procedure and the rule's lambda, so the args produced
    ; by minikanren match up.
    (define (collect-logic-vars tree acc)
      (syntax-case tree ()
        ((a . d) (collect-logic-vars #'d (collect-logic-vars #'a acc)))
        (id (identifier? #'id)
            (if (and (logic-var? #'id)
                     (not (find (lambda (v) (bound-identifier=? v #'id)) acc)))
                (cons #'id acc)
                acc))
        (_ acc)))

    ; Conditions are eventually quasi-quoted by dl-findo, so anything we
    ; want evaluated (logic vars and `this`) is wrapped in (unquote ...)
    ; here. Everything else stays literal — the datalog attr keys and the
    ; literal structural skeleton around bound positions.
    (define (xform-cond c)
      (syntax-case c ()
        ((a . d) (cons (xform-cond #'a) (xform-cond #'d)))
        (id (and (identifier? #'id)
                 (or (logic-var? #'id)
                     (free-identifier=? #'id #'this)))
            (list #'unquote #'id))
        (other #'other)))

    (syntax-case stx (do)
      ((_ ((cx condition cy) ...) do statement ...)
       (let* ((vars (reverse (collect-logic-vars
                               #'(((cx condition cy) ...) statement ...) '())))
              (conds* (xform-cond #'((cx condition cy) ...)))
              ; Semi-naive: the attr of each condition. If any contains a
              ; logic var we can't predict at expand time which attrs the
              ; rule depends on — mark as 'any so the rule is always
              ; eligible. datum->syntax so the template's #, splice gets a
              ; proper syntax object, not a raw datum (which the expander
              ; would reject as "raw symbol in macro output").
              (body-attrs (syntax->datum #'(condition ...)))
              (rule-attrs (datum->syntax stx
                            (if (any contains-logic-var? body-attrs)
                                'any
                                body-attrs))))
         #`(let* ((code (lambda (self #,@vars)
                          (syntax-parameterize
                              ((this  (identifier-syntax self))
                               (Claim (syntax-rules ()
                                        ((_ i a v) (derived-claim! self i a v))))
                               (Wish  (syntax-rules ()
                                        ((_ i a v) (derived-wish! self i a v))))
                               (When  (lambda (s)
                                        (syntax-violation
                                          'When "nested When is not supported" s))))
                            statement ...)))
                  (code-name (gensym))
                  (rule (fresh-vars #,(+ 1 (length vars))
                          (lambda (q #,@vars)
                            (conj (equalo q (list this 'code
                                                  (cons code-name (list #,@vars))))
                                  (dl-findo (get-dl) #,conds*))))))
             (hash-set! *rule-procs* code-name code)
             (dl-assert! (get-dl) this 'rules rule)
             (dl-assert-rule! (get-dl) rule '#,rule-attrs)))))))

; Used by the When macro to implement Claim/Wish *inside* a rule body.
; Mirror of Claim/Wish but routed through dl-assert-derived! so writes
; land in the IDB (re-derived next iteration if the rule still fires,
; retracted by the next dl-fixpoint! reset when it stops).
;
; Not part of the public API — user code should just write (Claim ...) /
; (Wish ...) inside a When body and let the parameterization do the routing.
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
;              are re-routed by When as usual.
;
; Consequence: results lag one frame. Chaining Collects stacks the lag.
;
; Because When is hygienic (bodies pass through untouched), ordinary
; template identifiers do all the bookkeeping: cur/prev/tt/swap! resolve
; to this expansion's defines and can't collide across Collect uses, and
; the injected ?collect-clock is bound-identifier-distinct from any
; user-written ?collect-clock even though both spell the same symbol.
; ----------------------------------------------------------------------
(define-syntax Collect
  (syntax-rules (emit as do)
    ((_ (cnd ...) emit e as name do body ...)
     (begin
       (define cur '())
       (define prev '())
       (define tt 0)
       (define (swap! t)
         (if (not (equal? t tt))
             (begin (set! prev cur)
                    (set! cur '())
                    (set! tt t))))
       ; MAP rule: user conds + injected clock; swap then accumulate.
       (When (cnd ... (time now ?collect-clock)) do
         (swap! ?collect-clock)
         (set! cur (cons e cur)))
       ; REDUCE rule: clock only; swap, bind name=prev, run body.
       (When ((time now ?collect-clock)) do
         (swap! ?collect-clock)
         (let ((name prev))
           body ...))))))
