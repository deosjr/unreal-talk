; ----------------------------------------------------------------------
; The fixpoint evaluator.
;
; Overrides datalog.scm's dl-fixpoint! to inject rule-body execution: each
; iteration accumulates new (this 'code (proc . args)) tuples, then applies
; the stored procs to run the bodies' side effects. MUST load after
; datalog.scm (last define wins) and after dsl.scm / state.scm whose
; *rule-procs* it reads.
;
; Semi-naive: only re-evaluate rules whose body attrs intersect with the
; deltas of the previous iteration. See datalog.scm for the bookkeeping
; (*delta-attrs*, rules-to-evaluate).
; ----------------------------------------------------------------------

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
