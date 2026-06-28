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

(define (Remember on id key value)
  (Forget on id key) ; always replace
  (hash-set! *remember* (list on id key) value))

(define (Forget on id key)
  (let* ((memkey (list on id key))
         (value (hash-ref *memories* memkey))
         (dbkey (list id key value)))
  (set! *forget* (cons dbkey *forget*))
  (hash-remove! *memories* memkey)))

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
