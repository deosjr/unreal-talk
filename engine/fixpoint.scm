; ----------------------------------------------------------------------
; Rule-body execution.
;
; datalog.scm's fixpoint calls *fixpoint-new-facts-hook* with each
; iteration's newly derived tuples after they are indexed. When rules
; (engine/dsl.scm) derive (this code (proc . args)) tuples; this hook
; recognizes them and applies proc — that's where the bodies' side
; effects run. The new-tuple dedup upstream means a body fires once per
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
                   (pair? (cddr c)) (pair? (caddr c)) (procedure? (caaddr c)))
          (let ((this (car c))
                (proc (caaddr c))
                (args (cdaddr c)))
            (catch #t
              (lambda () (apply proc this args))
              (lambda (key . eargs)
                (format (current-error-port)
                        "rule body error in page ~a: ~s ~s~%" this key eargs)
                (dl-assert-derived! dl this 'has-error
                  (format #f "rule: ~a: ~s" key eargs)))))))
      new)))
