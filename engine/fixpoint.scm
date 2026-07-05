; ----------------------------------------------------------------------
; Rule-body execution.
;
; datalog.scm's fixpoint calls *fixpoint-new-facts-hook* with each
; iteration's newly derived tuples after they are indexed. When rules
; (engine/dsl.scm) derive (this code (rule-id proc . args)) tuples; this
; hook recognizes them and applies proc — that's where the bodies' side
; effects run — and notes the firing against rule-id for the per-frame
; stats. The new-tuple dedup upstream means a body fires once per
; distinct binding per fixpoint. Tuples of any other shape (pure
; dl-rule! heads) pass through untouched.
;
; MUST load after datalog.scm, whose hook variable it sets.
;
; Each body invocation is wrapped: an exception in one rule body must
; not abort the fixpoint or escape into Go. The error is logged and
; asserted as a derived fact so other pages (e.g. an error-display page)
; can react.
; ----------------------------------------------------------------------

(set! *fixpoint-new-facts-hook*
  (lambda (dl new)
    (for-each
      (lambda (c)
        (when (and (pair? c) (pair? (cdr c)) (eq? (cadr c) 'code)
                   (pair? (cddr c)) (pair? (caddr c))
                   (pair? (cdaddr c)) (procedure? (cadr (caddr c))))
          (let* ((this (car c))
                 (payload (caddr c))     ; (rule-id proc . args)
                 (rule-id (car payload))
                 (proc (cadr payload))
                 (args (cddr payload)))
            (dl-note-fired! rule-id)
            (catch #t
              (lambda () (apply proc this args))
              (lambda (key . eargs)
                (let ((msg (exception-message key eargs)))
                  (format (current-error-port)
                          "rule body error in page ~a: ~a~%" this msg)
                  (dl-assert-derived! dl this '(error runtime)
                    (string-append "rule: " msg))))))))
      new)))

; ----------------------------------------------------------------------
; Fixpoint statistics as facts.
;
; Publishes the PREVIOUS frame's counters (datalog.scm records them
; while the fixpoint runs) as engine-claimed facts, at frame start and
; never mid-fixpoint — a stats fact appearing during the monotone run
; would trigger rules and change the numbers being measured. Stats are
; one frame stale, the same contract as Collect. Vocabulary:
;
;   (fixpoint iterations ?n)      (fixpoint new-facts ?n)
;   (?rule-id (rule matches) ?n)  ; join derivations (relative load)
;   (?rule-id (rule fired) ?n)    ; body invocations (exact)
;   (?rule-id (rule time-ms) ?x)  ; wall time in the rule's joins
;
; Together with the registration claims from When — (?p rules ?rule-id),
; (?rule-id (rule source) ...), (?rule-id (rule attrs) ...) — this is
; what makes rule inspectors expressible as ordinary scripts (see
; scripts/33.scm). Only rules currently in the rdb are published, so
; stats for a page that just left the table vanish with it.
; ----------------------------------------------------------------------

(define (publish-fixpoint-stats)
  ; old per-rule stats facts: retract wholesale (rule set changes frame
  ; to frame, so replace-in-place isn't enough)
  (for-each (lambda (rn) (engine-retract! (car rn) '(rule matches) (cadr rn)))
            (dl-query dl ((?r (rule matches) ?n)) (list ?r ?n)))
  (for-each (lambda (rn) (engine-retract! (car rn) '(rule fired) (cadr rn)))
            (dl-query dl ((?r (rule fired) ?n)) (list ?r ?n)))
  (for-each (lambda (rn) (engine-retract! (car rn) '(rule time-ms) (cadr rn)))
            (dl-query dl ((?r (rule time-ms) ?n)) (list ?r ?n)))
  (engine-replace! 'fixpoint 'iterations *fixpoint-iterations*)
  (engine-replace! 'fixpoint 'new-facts *fixpoint-new-facts*)
  (hash-for-each
    (lambda (rule _)
      (let* ((id (dl-rule-id rule))
             (s (dl-rule-stats id)))
        (engine-assert! id '(rule matches) (vector-ref s 0))
        (engine-assert! id '(rule fired) (vector-ref s 1))
        (engine-assert! id '(rule time-ms) (vector-ref s 2))))
    (datalog-rdb (get-dl))))
