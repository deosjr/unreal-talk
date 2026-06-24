; Tests for datalog.scm — run with:  GUILE_AUTO_COMPILE=0 guile -L . datalog_test.scm
;
; NOTE: keep GUILE_AUTO_COMPILE=0. This file (include)s datalog.scm, and
; Guile's compiled cache for this file is NOT invalidated when only the
; *included* datalog.scm changes — so a cached .go would silently test stale
; code. Disabling auto-compile sidesteps that entirely.
;
; Two kinds of tests:
;   (check name actual expected)
;       Asserts CURRENT, must-not-break behavior. A failure exits non-zero.
;   (check-pending name actual expected)
;       Describes TARGET behavior for the Phase 0 spike (logic variables
;       inside a compound attr, e.g. matching (?p (region ?name) ?v)).
;       These are expected to FAIL on today's datalog.scm and to PASS once
;       the spike lands. They never fail the suite; instead, when one starts
;       passing the harness prints "promote" so we move it to (check ...).

(include "datalog.scm")

;;; ----------------------------------------------------------------------
;;; tiny test harness
;;; ----------------------------------------------------------------------

(define *pass* 0)
(define *fail* 0)
(define *pending-fail* 0)
(define *pending-promote* '())   ; names of pending tests that now pass

(define (obj->str x) (format #f "~s" x))

;; Canonicalize a result set: results come back in hash-iteration order,
;; so sort by printed representation for stable comparison.
(define (as-set lst)
  (sort lst (lambda (a b) (string<? (obj->str a) (obj->str b)))))

(define (check name actual expected)
  (if (equal? actual expected)
      (begin (set! *pass* (+ *pass* 1))
             (format #t "  ok    ~a~%" name))
      (begin (set! *fail* (+ *fail* 1))
             (format #t " FAIL   ~a~%        expected: ~s~%        actual:   ~s~%"
                     name expected actual))))

(define (check-pending name actual expected)
  (if (equal? actual expected)
      (begin (set! *pending-promote* (cons name *pending-promote*))
             (format #t " PEND*  ~a   <-- now PASSING; promote to (check ...)~%" name))
      (begin (set! *pending-fail* (+ *pending-fail* 1))
             (format #t " pend   ~a   (expected to fail until Phase 0 lands)~%" name))))

;;; ----------------------------------------------------------------------
;;; query helpers
;;; ----------------------------------------------------------------------

;; All values V such that (E ATTR V) holds, for ground E and ground ATTR.
(define (q-values dl e attr)
  (as-set (dl-find (fresh-vars 1 (lambda (v) (dl-findo dl ( (,e ,attr ,v) )))))))

;; All entities E such that (E ATTR _) holds, for ground ATTR.
(define (q-entities dl attr)
  (as-set (dl-find (fresh-vars 2 (lambda (e v) (dl-findo dl ( (,e ,attr ,v) )))))))

;; All (attr . value) pairs for a ground entity E (attr left as a variable).
(define (q-attrs dl e)
  (as-set (dl-find (fresh-vars 3
            (lambda (q a v)
              (conj+ (equalo q (cons a v))
                     (dl-findo dl ( (,e ,a ,v) ))))))))

;; All (regionName . value) pairs for ground entity E where the attr is the
;; PARTIAL compound (region <var>). This is the Phase 0 target shape.
(define (q-regions-of dl e)
  (as-set (dl-find (fresh-vars 3
            (lambda (q n v)
              (conj+ (equalo q (cons n v))
                     (dl-findo dl ( (,e (region ,n) ,v) ))))))))

;;; ----------------------------------------------------------------------
;;; fixture
;;; ----------------------------------------------------------------------

;; A fresh datalog with a couple of "pages" carrying named regions plus one
;; non-region attr (card-type) that partial-attr queries must NOT pick up.
(define (make-fixture)
  (let ((dl (make-new-datalog)))
    (dl-assert! dl 'p1 '(region page-points) 'pp1)
    (dl-assert! dl 'p1 '(region editor)      'ed1)
    (dl-assert! dl 'p1 '(region outline)     'ol1)
    (dl-assert! dl 'p1 'attribute            'value)
    (dl-assert! dl 'p2 '(region page-points) 'pp2)
    (dl-assert! dl 'p2 '(region editor)      'ed2)
    dl))

;;; ----------------------------------------------------------------------
;;; regression tests — current behavior that the spike must preserve
;;; ----------------------------------------------------------------------

(let ((dl (make-fixture)))
  ;; entity bound + attr fully ground -> exact EAV lookup
  (check "exact: p1 (region editor)"
         (q-values dl 'p1 '(region editor)) '(ed1))

  ;; entity unbound + attr ground -> attr index
  (check "attr-index: who has (region editor)"
         (q-entities dl '(region editor)) '(p1 p2))
  (check "attr-index: who has (region outline)"
         (q-entities dl '(region outline)) '(p1))

  ;; entity bound + attr a (bare) variable -> entity index, all of p1's tuples
  (check "entity-index: all attrs of p1"
         (q-attrs dl 'p1)
         (as-set (list (cons '(region page-points) 'pp1)
                       (cons '(region editor)      'ed1)
                       (cons '(region outline)     'ol1)
                       (cons 'attribute            'value))))

  ;; ground compound attr must yield exactly one tuple (guards against the
  ;; refactor accidentally firing two conde branches and duplicating).
  (check "no-dup: p1 (region page-points) is single"
         (q-values dl 'p1 '(region page-points)) '(pp1)))

;; retract removes a fact from every index it was added to
(let ((dl (make-fixture)))
  (dl-retract! dl (list 'p1 '(region outline) 'ol1))
  (check "retract: (region outline) gone from attr index"
         (q-entities dl '(region outline)) '())
  (check "retract: (region outline) gone from eav"
         (q-values dl 'p1 '(region outline)) '())
  (check "retract: sibling region untouched"
         (q-values dl 'p1 '(region editor)) '(ed1)))

;; semi-naive fixpoint: transitive closure (mirrors the example in datalog.scm)
(let ((dl (make-new-datalog)))
  (define a (dl-record! dl 'vertex))
  (define b (dl-record! dl 'vertex))
  (define c (dl-record! dl 'vertex))
  (define d (dl-record! dl 'vertex))
  (define e (dl-record! dl 'vertex))
  (define (edge x y) (dl-assert! dl x 'edge y))
  (edge a c) (edge b a) (edge b d) (edge c d) (edge d a) (edge d e)
  (dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?y))
  (dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))
  (dl-fixpoint! dl)
  ;; nodes reachable from b
  (check "fixpoint: reachable from b"
         (as-set (dl-find (fresh-vars 2 (lambda (y _) (dl-findo dl ( (,b reachable ,y) ))))))
         (as-set (list a c d e)))
  ;; self-reachable cycle members (the doc example): a, c, d
  (check "fixpoint: nodes on a cycle (self-reachable)"
         (as-set (dl-find (fresh-vars 1 (lambda (id) (dl-findo dl ( (,id reachable ,id) ))))))
         (as-set (list a c d))))

;;; ----------------------------------------------------------------------
;;; Phase 0 — logic var inside a compound attr (promoted from pending once
;;; the spike landed; these now guard against regressing it)
;;; ----------------------------------------------------------------------

;; (1) Direct query: bound entity + PARTIAL compound attr (region ?n).
;;     Must return every region of p1 and bind ?n, and must EXCLUDE the
;;     non-region card-type tuple.
(let ((dl (make-fixture)))
  (check "partial-attr: all regions of p1 (excludes card-type)"
         (q-regions-of dl 'p1)
         (as-set (list (cons 'page-points 'pp1)
                       (cons 'editor      'ed1)
                       (cons 'outline     'ol1)))))

;; (2) Rule eligibility + evaluation through a partial compound attr.
;;     The rule binds the entity first (kind page), then matches its regions
;;     via (region ?n). Requires BOTH spike halves: semi-naive must schedule a
;;     rule whose attr contains a logic var, and dl-findo_ must match the
;;     bound-entity/partial-attr shape.
(let ((dl (make-fixture)))
  (dl-assert! dl 'p1 'kind 'page)
  (dl-assert! dl 'p2 'kind 'page)
  (dl-rule! dl (found-region ,?p ,?n) :- (kind ,?p page) ((region ,?n) ,?p ,?c))
  (dl-fixpoint! dl)
  (check "rule: derive (?p found-region ?n) for every page region"
         (as-set (dl-find (fresh-vars 3
                   (lambda (q p n)
                     (conj+ (equalo q (cons p n))
                            (dl-findo dl ( (,p found-region ,n) )))))))
         (as-set (list (cons 'p1 'page-points) (cons 'p1 'editor) (cons 'p1 'outline)
                       (cons 'p2 'page-points) (cons 'p2 'editor)))))

;;; ----------------------------------------------------------------------
;;; Fully-open queries — unbound entity AND non-ground attr. Exercises the
;;; "bind the entity first" planner branch (enumerate entities from idx-entity,
;;; then match via the entity index). This is the whisker shape:
;;;   (?q (region ?name) ?v)
;;; ----------------------------------------------------------------------

;; (1) Open entity + PARTIAL compound attr: every (entity . region-name) pair
;;     across all pages, binding both the entity and the region name, while
;;     excluding the non-region (attribute . value) tuple.
(let ((dl (make-fixture)))
  (check "open: every (entity . region-name) across all pages"
         (as-set (dl-find (fresh-vars 4
                   (lambda (q e n v)
                     (conj+ (equalo q (cons e n))
                            (dl-findo dl ( (,e (region ,n) ,v) )))))))
         (as-set (list (cons 'p1 'page-points) (cons 'p1 'editor) (cons 'p1 'outline)
                       (cons 'p2 'page-points) (cons 'p2 'editor)))))

;; (2) Fully-open triple: unbound entity, unbound (bare-var) attr. Returns the
;;     ENTIRE fact set exactly once — including the non-region tuple — with no
;;     duplicates (guards against the new branch overlapping the others).
(let ((dl (make-fixture)))
  (check "open: the whole database, once each"
         (as-set (dl-find (fresh-vars 4
                   (lambda (q e a v)
                     (conj+ (equalo q (list e a v))
                            (dl-findo dl ( (,e ,a ,v) )))))))
         (as-set (list (list 'p1 '(region page-points) 'pp1)
                       (list 'p1 '(region editor)      'ed1)
                       (list 'p1 '(region outline)     'ol1)
                       (list 'p1 'attribute            'value)
                       (list 'p2 '(region page-points) 'pp2)
                       (list 'p2 '(region editor)      'ed2)))))

;;; ----------------------------------------------------------------------
;;; summary / exit code
;;; ----------------------------------------------------------------------

(format #t "~%~a passed, ~a failed | pending: ~a still-failing, ~a now-passing~%"
        *pass* *fail* *pending-fail* (length *pending-promote*))
(when (not (null? *pending-promote*))
  (format #t "PROMOTE these pending tests to (check ...):~%")
  (for-each (lambda (n) (format #t "  - ~a~%" n)) *pending-promote*))
(exit (if (> *fail* 0) 1 0))
