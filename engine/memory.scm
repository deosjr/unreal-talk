; ----------------------------------------------------------------------
; Memories.
;
; Remember/Forget can store state beyond a page's lifetime.
; For now we will use an in-memory db so state is still scoped to RealTalkOS lifetime
; https://dynamicland.org/archive/2020/Memories
; "The remembered statements on an object are only there when the object itself is there and running"
; Values are unique per key, for now. Replacing is automatic.
; Page-local vars are still a thing too, they are just less reliable since detection isn't 100% stable
; ----------------------------------------------------------------------

(define *memories* (make-hash-table))
(define *forget* '()) ; stages memories to forget; cleaned up each iteration
(define *remember* (make-hash-table)) ; stages memories to remember

; #:default makes the write conditional: it only lands when the key is
; neither stored in *memories* nor already staged this frame. Checking
; the stores rather than the db keeps it deterministic (memories only
; change between frames, while db facts appear mid-fixpoint) and closes
; the rule-ordering race where an update staged earlier in the same
; fixpoint would be clobbered by the default. hash-get-handle so a
; remembered #f still counts as assigned.
;
; So at page top level: plain Remember means "reset on every arrival",
; #:default #t means "seed once, persist thereafter".
(define* (remember! on id key value #:key (default #f))
  (unless (and default
               (or (hash-get-handle *memories* (list on id key))
                   (hash-get-handle *remember* (list on id key))))
    (forget! on id key) ; always replace
    (hash-set! *remember* (list on id key) value)))

(define (forget! on id key)
  (let* ((memkey (list on id key))
         (value (hash-ref *memories* memkey))
         (dbkey (list id key value)))
  (set! *forget* (cons dbkey *forget*))
  (hash-remove! *memories* memkey)))

; Remember/Forget are macros so the common self-memory case can omit
; on/id: (Remember key value) means (Remember this this key value),
; resolved through the dsl's `this` syntax parameter — works at page
; top level and inside When bodies, expand-time error outside page
; code. The explicit form remains for cross-entity memories, where
; `on` decides whose presence the memory is tied to. The keyword
; patterns must come first or the 4-element #:default short form
; would be swallowed by the explicit plain form.
(define-syntax Remember
  (syntax-rules ()
    ((_ key value #:default d)       (remember! this this key value #:default d))
    ((_ on id key value #:default d) (remember! on id key value #:default d))
    ((_ key value)                   (remember! this this key value))
    ((_ on id key value)             (remember! on id key value))))

(define-syntax Forget
  (syntax-rules ()
    ((_ key)       (forget! this this key))
    ((_ on id key) (forget! on id key))))

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
