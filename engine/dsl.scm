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
; and nothing else. See datalog.scm for the engine these sit on top of
; (rule records, the join, semi-naive evaluation), and fixpoint.scm for
; the hook that actually runs rule bodies.
; ----------------------------------------------------------------------

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
; A rule with side effects. Compiles to a <dl-rule> (datalog.scm) whose
; produce emits, per match, the tuple
;
;   (this 'code (body-proc ?var1-value ?var2-value ...))
;
; with body-proc the rule's body wrapped as (lambda (self ?var1 ...)).
; The fixpoint's new-tuple dedup means the body runs once per distinct
; binding per fixpoint; engine/fixpoint.scm's hook recognizes the tuple
; shape and applies body-proc — the When macro never executes the body
; directly.
;
; Condition surface syntax: a small DSL where
;   ?var          -> a pattern variable (bound by matching)
;   this          -> the page id (the syntax parameter above)
;   anything else -> a literal datum (used as the datalog attr or as
;                    a literal structural component of entity/value)
; No commas needed. The body, in contrast, is plain Scheme — pattern
; vars there are just lexical bindings introduced by the body lambda.
;
; Implementation: pattern vars like ?x ARE ordinary Scheme identifiers,
; collected from the conditions as syntax objects (bound-identifier=?
; dedup keeps a macro-introduced ?x distinct from a user ?x) and spliced
; verbatim as the body lambda's parameters — the body passes through
; untouched, with full hygiene. The pattern itself carries slot indices
; (<pvar> records via pattern-template in datalog.scm), so variable
; NAMES never matter at runtime. A ?var used in the body but not in any
; condition has nothing to bind it — that's an expand-time error.
; ----------------------------------------------------------------------
(define-syntax-parameter When
  (lambda (stx)
    ; Reject a literal nested When up front — otherwise the body-var
    ; check below trips over the inner rule's fresh variables first and
    ; reports a misleading "not bound by the conditions" error. (Whens
    ; introduced into the body by other macros are still caught by the
    ; syntax-parameterize in the emitted code.)
    (define (check-no-nested-when tree)
      (syntax-case tree (quote)
        ((quote _) #t)
        ((head . rest)
         (begin
           (when (and (identifier? #'head)
                      (free-identifier=? #'head #'When))
             (syntax-violation 'When "nested When is not supported" stx))
           (check-no-nested-when #'head)
           (check-no-nested-when #'rest)))
        (_ #t)))
    (syntax-case stx (do)
      ((_ ((cx condition cy) ...) do statement ...)
       (check-no-nested-when #'(statement ...))
       (let ((cond-vars (reverse (collect-pattern-vars #'((cx condition cy) ...) '())))
             (body-vars (reverse (collect-pattern-vars #'(statement ...) '()))))
         (for-each
           (lambda (bv)
             (unless (find (lambda (v) (bound-identifier=? v bv)) cond-vars)
               (syntax-violation 'When
                 "logic variable in body is not bound by the conditions" stx bv)))
           body-vars)
         (let* ((tpl (pattern-template #'((cx condition cy) ...) cond-vars))
                (n   (length cond-vars))
                ; Semi-naive eligibility: the attr of each condition, or
                ; 'any when an attr contains a variable (unpredictable at
                ; expand time -> always eligible).
                (body-attrs (syntax->datum #'(condition ...)))
                (rule-attrs (if (any contains-logic-var? body-attrs)
                                'any body-attrs)))
           #`(let* ((code (lambda (self #,@cond-vars)
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
                    (rule (make-dl-rule
                            #,(list #'quasiquote tpl)
                            #,n
                            ; args in slot order = cond-vars order = the
                            ; body lambda's parameter order
                            (lambda (env) (list this 'code (cons code (vector->list env))))
                            '#,(datum->syntax stx rule-attrs)
                            '#,(datum->syntax stx (syntax->datum #'((cx condition cy) ...))))))
               (dl-assert! (get-dl) this 'rules rule)
               (dl-assert-rule! (get-dl) rule))))))))

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
