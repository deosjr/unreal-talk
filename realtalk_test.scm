; Tests for the RealTalk DSL (engine/dsl.scm) — run with:
;   GUILE_AUTO_COMPILE=0 guile -L . realtalk_test.scm
;
; NOTE: keep GUILE_AUTO_COMPILE=0. This file (include)s realtalk.scm, which
; in turn (include)s datalog.scm and the engine/*.scm modules. Guile's
; compiled cache for THIS file is NOT invalidated when only an *included*
; file changes — so a cached .go would silently test stale code. Disabling
; auto-compile sidesteps that entirely.
;
; These tests drive the DSL the way the engine does: build a page proc with
; make-page-code, run it once to register its rules, then assert facts and
; call dl-fixpoint! to derive results. We exercise:
;   * When   — a rule body that Claims one fact per match.
;   * Collect — cross-frame map-reduce, including the negation/forall case
;               where the reduce body must fire even on an empty set, and
;               the one-frame latency that makes the previous frame visible.

(include "realtalk.scm")

;;; ----------------------------------------------------------------------
;;; tiny test harness (mirrors datalog_test.scm)
;;; ----------------------------------------------------------------------

(define *pass* 0)
(define *fail* 0)

(define (obj->str x) (format #f "~s" x))

(define (as-set lst)
  (sort lst (lambda (a b) (string<? (obj->str a) (obj->str b)))))

(define (check name actual expected)
  (if (equal? actual expected)
      (begin (set! *pass* (+ *pass* 1))
             (format #t "  ok    ~a~%" name))
      (begin (set! *fail* (+ *fail* 1))
             (format #t " FAIL   ~a~%        expected: ~s~%        actual:   ~s~%"
                     name expected actual))))

;;; ----------------------------------------------------------------------
;;; helpers
;;; ----------------------------------------------------------------------

;; All values V such that (E ATTR V) holds in the global dl.
(define (vals e attr)
  (as-set (dl-query dl ((,e ,attr ?v)) ?v)))

;; Set (time now) to T (single-valued: retract any previous tick first).
(define (set-time! t)
  (for-each (lambda (c) (dl-retract! dl `(time now ,c)))
            (dl-query dl ((time now ?x)) ?x))
  (dl-assert! dl 'time 'now t))

;;; ----------------------------------------------------------------------
;;; When — one derived Claim per matching tuple
;;; ----------------------------------------------------------------------

(define when-proc
  (make-page-code
    (When ((?p kind widget)) do (Claim this 'saw ?p))))

(when-proc 'wp)                 ; register the rule
(dl-assert! dl 'a 'kind 'widget)
(dl-assert! dl 'b 'kind 'widget)
(dl-assert! dl 'c 'kind 'gizmo)   ; must NOT match
(dl-fixpoint! dl)

(check "When: derives one Claim per matching tuple"
       (vals 'wp 'saw) (as-set '(a b)))

;; registration also claims rule-introspection facts, so filter to 'saw
(check "When: also records the claim under (this claims ...)"
       (as-set (filter (lambda (c) (eq? (cadr c) 'saw)) (vals 'wp 'claims)))
       (as-set (list (list 'wp 'saw 'a) (list 'wp 'saw 'b))))

;;; ----------------------------------------------------------------------
;;; Collect — cross-frame map-reduce with the empty-set (negation) case
;;;
;;; The reduce body Claims the count of the PREVIOUS frame's matches, and
;;; Claims 'isempty when that set was empty. Because the consumer is keyed
;;; on (time now) it fires once per frame even when nothing matched — that
;;; is what makes the empty case observable.
;;; ----------------------------------------------------------------------

(define collect-proc
  (make-page-code
    (Collect ((?p kind gadget))
      emit ?p
      as   gs
      do   (begin
             (Claim this 'gcount (length gs))
             (if (null? gs) (Claim this 'isempty #t))))))

(collect-proc 'cg)              ; register accumulator + consumer rules

(dl-assert! dl 'x 'kind 'gadget)
(dl-assert! dl 'y 'kind 'gadget)

;; Frame 1: prev buffer is still empty -> count 0, isempty fires.
(set-time! 1)
(dl-fixpoint! dl)
(check "Collect: frame 1 sees empty previous set (negation fires)"
       (vals 'cg 'gcount) '(0))
(check "Collect: frame 1 Claims isempty on the empty set"
       (vals 'cg 'isempty) '(#t))

;; Frame 2: prev now holds frame 1's fully-converged set of 2 gadgets.
(set-time! 2)
(dl-fixpoint! dl)
(check "Collect: frame 2 sees the previous frame's full set"
       (vals 'cg 'gcount) '(2))
(check "Collect: frame 2 no longer Claims isempty (set non-empty)"
       (vals 'cg 'isempty) '())

;;; ----------------------------------------------------------------------
;;; Introspection — rules and fixpoint stats visible as facts.
;;;
;;; Registration claims (?p rules ?r) / (?r (rule source) ...) as the
;;; page; publish-fixpoint-stats (scene.scm) engine-claims the previous
;;; fixpoint's counters. In the live system publication happens at frame
;;; start in receive-pages-found; here we call it directly after the
;;; last dl-fixpoint! above (frame 2 of the Collect test).
;;; ----------------------------------------------------------------------

(define wp-rules (dl-query dl ((wp rules ?r)) ?r))

(check "introspect: page wp owns exactly one rule" (length wp-rules) 1)
(check "introspect: page cg owns two rules (Collect = map + reduce)"
       (length (dl-query dl ((cg rules ?r)) ?r)) 2)

(define wp-rule (car wp-rules))

(check "introspect: rule source is the full When form"
       (car (car (dl-query dl ((,wp-rule (rule source) ?s)) ?s)))
       'When)
(check "introspect: rule attrs claimed"
       (dl-query dl ((,wp-rule (rule attrs) ?a)) ?a)
       '((kind)))

(publish-fixpoint-stats)

(check "introspect: fired count = body invocations last fixpoint"
       (dl-query dl ((,wp-rule (rule fired) ?n)) ?n)
       '(2))
(check "introspect: fixpoint iteration count is a positive fact"
       (positive? (car (dl-query dl ((fixpoint iterations ?n)) ?n)))
       #t)
(check "introspect: stats facts are engine-claimed"
       (not (null? (dl-query dl ((engine claims (,wp-rule (rule fired) ?n))) ?n)))
       #t)

;;; teardown: a page leaving takes its rules AND their metadata with it
(page-moved-from-table 'wp)
(check "introspect: teardown retracts rule metadata"
       (append (dl-query dl ((wp rules ?r)) ?r)
               (dl-query dl ((,wp-rule (rule source) ?s)) ?s))
       '())

;;; ----------------------------------------------------------------------
;;; summary / exit code
;;; ----------------------------------------------------------------------

(format #t "~%~a passed, ~a failed~%" *pass* *fail*)
(exit (if (> *fail* 0) 1 0))
