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
  (assert-time)
  (dl-fixpoint! (get-dl)))

; TODO: assert engine facts like time and key with ?e = 'editor, so they can be queried
; i.e. 'which facts does the engine provide?' even page points should have 'engine claims ...'

; note: guile scheme gettimeofday returns a pair of seconds and microseconds in unix epoch
(define (assert-time)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time now ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time now ,claim))) claims))
  (dl-assert! dl 'time 'now (gettimeofday)))

(define (assert-time-detect d)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time detect ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time detect ,claim))) claims))
  (dl-assert! dl 'time 'detect d))

(define (assert-time-scm d)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (time scm ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(time scm ,claim))) claims))
  (dl-assert! dl 'time 'scm d))

; if key == -1 then no key was pressed
(define (assert-key key)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (key down ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(key down ,claim))) claims))
  (if (not (= key -1)) (dl-assert! dl 'key 'down key)))
