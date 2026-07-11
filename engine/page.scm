; ----------------------------------------------------------------------
; Page lifecycle + loading.
;
; Creating page ids, mirroring page geometry into datalog, running a page's
; init code when it arrives on the table, and tearing its facts down when
; it leaves. The lower half of the file is the compile/load side: reading
; scripts from disk, wrapping them in (make-page-code ...) and evaluating
; them into page procedures, plus saving edits back out.
; ----------------------------------------------------------------------

; pid -> the compiled page procedure (lambda (this) ...). Populated by
; add-page / load-page / save-page; run by execute-page.
(define *procs* (make-hash-table))
; ids we've already tried to load. Tried-once-and-failed entries stay in
; the set so we don't re-read a missing file every fixpoint when a tag is
; on the table — the (error io) fact does the messaging.
(define *load-attempted* (make-hash-table))

(define (make-page-id) (dl-record dl 'page))

(define (add-page proc)
  (let* ((pid (dl-record! dl 'page ('code proc))))
    (hash-set! *procs* pid proc)
    pid))

; make page dimensions known in datalog. Geometry comes from the camera,
; not from any page, so it is claimed by the engine (engine-assert! in
; scene.scm) — a provenance query on page points answers 'engine.
(define (update-page-geometry pid points rotation)
    (retract-page-geometry pid)
    (engine-assert! pid '(page points) points)
    (engine-assert! pid '(page rotation) rotation))

(define (retract-page-geometry pid)
  (let ((points   (dl-query dl ((,pid (page points) ?x)) ?x))
        (rotation (dl-query dl ((,pid (page rotation) ?x)) ?x)))
    (if (not (null? points)) (engine-retract! pid '(page points) (car points)))
    (if (not (null? rotation)) (engine-retract! pid '(page rotation) (car rotation)))))

; Clear any (error <class>) facts attached to PID, including their
; engine-claims bookkeeping. Used both on successful recompile (to drop
; a stale error sticking around from a previous bad save) and when a
; page leaves the table (cleanup). Also sweeps derived rule-error facts
; (fixpoint.scm); those never had an engine-claims twin, but dl-retract!
; ignores missing tuples so the extra retract is harmless.
(define (dl-retract-page-errors! pid)
  (for-each (lambda (cm) (engine-retract! pid `(error ,(car cm)) (cadr cm)))
            (dl-query dl ((,pid (error ?c) ?m)) (list ?c ?m))))

; Single writer for page error facts: (pid (error class) msg), class one
; of compile/runtime/io. Replaces any previous error on the page. Routed
; through engine-assert! so provenance queries have an answer — (engine
; claims (pid (error class) msg)) — like every other engine-side fact.
(define (assert-page-error! pid class msg)
  (dl-retract-page-errors! pid)
  (engine-assert! pid `(error ,class) msg))

; Render a caught (key . args) the way the REPL would — "Unbound
; variable: foo" — instead of dumping the raw (subr fmt fmt-args data)
; throw tuple. Nonstandard keys degrade to "Throw to key ... with args
; ...". Shared by every error handler here and in fixpoint.scm.
(define (exception-message key args)
  (string-trim-right
    (call-with-output-string
      (lambda (port) (print-exception port #f key args)))))

; Retract any existing (pid (page code) _) facts. Used by load-page and
; save-page before asserting a new value, so the (page code) attribute
; behaves like a single-valued slot rather than accumulating every
; version the page has ever had.
(define (dl-retract-page-code! pid)
  (for-each (lambda (str) (dl-retract! dl `(,pid (page code) ,str)))
            (dl-query dl ((,pid (page code) ?x)) ?x)))

; only run page code when newly in bounds of table.
; Wrapped: page top-level code that throws (bad Claim args, undefined
; helpers, etc.) must not crash the system. Old proc stays in *procs*
; until next save; error fact is asserted so it can be displayed.
; TODO: Claim (p) is present, work from there
(define (page-moved-onto-table pid)
  (catch #t
    (lambda () (execute-page pid))
    (lambda (key . args)
      (let ((msg (exception-message key args)))
        (format (current-error-port)
                "page init error in page ~a: ~a~%" pid msg)
        (assert-page-error! pid 'runtime (string-append "init: " msg))))))

; then retract all 'this claims x' and 'this rules x' from dl-db when newly out of table bounds
(define (page-moved-from-table pid)
  (retract-page-geometry pid)
  (dl-retract-page-errors! pid)
  (let ((claims (dl-query dl ((,pid claims ?x)) ?x))
        (wishes (dl-query dl ((,pid wishes ?x)) ?x))
        (rules  (dl-query dl ((,pid rules ?x)) ?x)))
    (for-each (lambda (claim) (dl-retract! dl claim)) claims)
    (for-each (lambda (claim) (dl-retract! dl `(,pid claims ,claim))) claims)
    (for-each (lambda (wish) (dl-retract! dl `(,pid wishes ,wish))) wishes)
    ; rules shows rule-IDS (the page's introspection claims); the claims
    ; loop above already retracted those facts — here we retract the rule
    ; RECORDS from the rdb and clean the id-keyed side tables.
    (for-each (lambda (rule-id)
                (let ((rule (hash-ref *rules-by-id* rule-id #f)))
                  (if rule (dl-retract-rule! dl rule)))
                (hash-remove! *rules-by-id* rule-id)
                (dl-drop-rule-stats! rule-id))
              rules)))

(define (execute-page pid)
  (let ((proc (hash-ref *procs* pid #f)))
    (if proc (proc pid))))

; ----------------------------------------------------------------------
; Loading / compilation: turning script source text into a page proc.
; ----------------------------------------------------------------------

(define (read-page-code id)
  (call-with-input-file (format #f "scripts/~d.scm" id) (lambda (port)
    (get-string-all port)) #:encoding "utf-8"))

; Wrap user code in (make-page-code ...) and eval it, returning the
; resulting page procedure. On any error: log, assert an (error compile)
; fact on the page (visible to error-display pages), and return #f so
; the caller can decide what to do (load-page skips; save-page keeps the
; old proc in place).
(define (try-compile-page id phase code-str)
  (catch #t
    (lambda () (eval-string (format #f "(make-page-code ~a)" code-str)))
    (lambda (key . args)
      (let ((msg (exception-message key args)))
        (format (current-error-port)
                "~a error in page ~a: ~a~%" phase id msg)
        (assert-page-error! id 'compile
          (format #f "~a: ~a" phase msg))
        #f))))

; Empty source we assert as (page code) when the script file is missing
; so the editor (scripts/1.scm) — whose `When` matches `(?p (page code)
; ?str)` — can still see the tag and let the user write code from
; scratch. A save replaces the placeholder via dl-retract-page-code!.
(define empty-script "")

; Read the script file then compile. Wrapped: a missing scripts/<id>.scm
; (or an unreadable one) should not crash main.scm and should surface as
; an (error io) fact so 9005 renders it at the offending tag. Compile
; errors are already handled by try-compile-page.
;
; Records the attempt in *load-attempted* so ensure-loaded! won't keep
; re-trying a missing file every fixpoint.
(define (load-page id)
  (hash-set! *load-attempted* id #t)
  (dl-retract-page-code! id)
  (let ((str (catch #t
               (lambda () (read-page-code id))
               (lambda (key . args)
                 (format (current-error-port)
                         "could not read script for tag ~a: ~a~%" id
                         (exception-message key args))
                 (assert-page-error! id 'io
                   (format #f "no script for tag ~a" id))
                 ; Assert an empty placeholder so the editor can pick up
                 ; the page despite the missing file.
                 (dl-assert! dl id '(page code) empty-script)
                 #f))))
    (if str
      (let ((proc (try-compile-page id 'load str)))
        (dl-assert! (get-dl) id '(page code) str)
        (if proc (hash-set! *procs* id proc))))))

; Lazy-load: try once per session per tag id. Subsequent placements of
; the same tag skip the file read (uses the cached *procs* entry, or
; respects a previous failed attempt). Called from receive-pages-found
; when a tag is first seen.
(define (ensure-loaded! id)
  (if (not (hash-ref *load-attempted* id #f))
    (load-page id)))

(define (save-page id code-str)
  (let ((proc (try-compile-page id 'save code-str)))
    ; If compile failed, leave *procs* and (page code) untouched so the
    ; page keeps running its last good version while the user fixes the
    ; new source — (error compile) was already asserted by try-compile-page.
    (if proc
      (begin
        (write-page-code id code-str)
        (dl-retract-page-errors! id)
        (dl-retract-page-code! id)
        (dl-assert! (get-dl) id '(page code) code-str)
        (hash-set! *procs* id proc)
        (page-moved-from-table id)
        (page-moved-onto-table id)))))

; mirror of read-page-code; atomic so a crash mid-write can't corrupt the db
(define (write-page-code id code-str)
  (let ((path (format #f "scripts/~d.scm" id))
        (tmp  (format #f "scripts/~d.scm.tmp" id)))
    (call-with-output-file tmp
      (lambda (port) (put-string port code-str))
      #:encoding "utf-8")
    (rename-file tmp path)))   ; atomic replace

; todo: once a background page is physically present on the table,
; it unloads once gone and takes all its effects with it :)
(define (load-background-page id)
  (load-page id)
  (page-moved-onto-table id)
  (set! *background-pages* (cons id *background-pages*)))
