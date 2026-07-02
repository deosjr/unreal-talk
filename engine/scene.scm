; ----------------------------------------------------------------------
; Scene registry and the per-frame entry point.
;
; Tracks which pages are physically on the table this frame vs. last, and
; drives the whole cycle from Go: receive-pages-found mirrors geometry,
; runs enter/leave hooks, updates memories, asserts engine facts (time,
; key) and kicks off the fixpoint. The assert-* helpers below publish the
; engine-provided facts that page rules can query. The image pointers and
; init-images live here too: they are the other half of the Go-side setup
; that the per-frame loop draws into (receive-pages-found fills projection).
; ----------------------------------------------------------------------

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
  (publish-fixpoint-stats)
  (assert-time)
  (dl-fixpoint! (get-dl)))

; ----------------------------------------------------------------------
; Engine-provided facts.
;
; The engine speaks the same protocol pages speak: every fact it asserts
; goes through the double bookkeeping Claim uses, so provenance queries
; have an answer — (engine claims (time now ...)), (engine claims
; (?p (page points) ...)), and so on. A generic "who claims this?" page
; works uniformly across user pages and the engine.
; ----------------------------------------------------------------------

(define (engine-assert! e a v)
  (dl-assert! dl 'engine 'claims (list e a v))
  (dl-assert! dl e a v))

(define (engine-retract! e a v)
  (dl-retract! dl (list 'engine 'claims (list e a v)))
  (dl-retract! dl (list e a v)))

; Retract every (E A _) fact and its engine-claims bookkeeping, then
; assert the new value: engine facts behave as single-valued slots.
(define (engine-replace! e a v)
  (for-each (lambda (old) (engine-retract! e a old))
            (dl-query dl ((,e ,a ?x)) ?x))
  (engine-assert! e a v))

; note: guile scheme gettimeofday returns a pair of seconds and microseconds in unix epoch
(define (assert-time)
  (engine-replace! 'time 'now (gettimeofday)))

(define (assert-time-detect d)
  (engine-replace! 'time 'detect d))

(define (assert-time-scm d)
  (engine-replace! 'time 'scm d))

; if key == -1 then no key was pressed
(define (assert-key key)
  (for-each (lambda (claim) (engine-retract! 'key 'down claim))
            (dl-query dl ((key down ?x)) ?x))
  (if (not (= key -1)) (engine-assert! 'key 'down key)))

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
