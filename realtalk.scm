(include "datalog.scm")
(use-modules (ice-9 threads))

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)
;
; This file is the orchestrator: it pulls in datalog.scm (the EAV engine:
; pattern matching, rule records, semi-naive fixpoint), creates the one
; datalog instance the whole engine wraps, and then loads the engine/
; modules that build the RealTalk layer on top.
; Everything loads into one flat namespace via `include` — there are no
; modules — so order matters in two ways:
;   * macros must be defined before any code that expands them, and
;   * fixpoint.scm sets datalog.scm's *fixpoint-new-facts-hook* (to run
;     When bodies), so it must come after datalog.scm.
; Plain function and global defines resolve at call time, so cross-file
; order among them is otherwise flexible.

; The single datalog instance every engine module reads/writes. Lives here
; (not in a module) because it is the root the whole layer wraps; defining
; it next to (include "datalog.scm") keeps the "one db" wiring in one place.
(define dl (make-new-datalog))
(define (get-dl) dl)

(include "engine/dsl.scm")      ; Claim/Wish/When/Collect macros
(include "engine/fixpoint.scm") ; new-facts hook (runs rule bodies)
(include "engine/memory.scm")   ; Remember/Forget + memory staging
(include "engine/page.scm")     ; page lifecycle + loading (*procs*, *load-attempted*)
(include "engine/scene.scm")    ; scene registry, image pointers, per-frame entry point
