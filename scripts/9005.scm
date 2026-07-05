; Surface (?p (error ?class) ?msg) facts as subtitles under the
; offending page.
;
; error facts are asserted by realtalk.scm's error handlers:
;   - (error runtime): rule body throws during fixpoint (derived fact,
;     auto-clears next fixpoint if the body stops throwing), or
;     page-moved-onto-table's init throws (EDB fact, cleared on next
;     good save or when the page leaves the table)
;   - (error compile): load/save eval-string fails (EDB fact, cleared
;     on next good save)
;   - (error io): missing or unreadable script file
;
; Reusing 9001's 'subtitled wish means error rendering goes through the
; same path as ordinary labels, and the subtitle vanishes for free when
; the error fact retracts. Full message stays in stderr; this is just
; the on-table glance value.

(define max-msg-len 60)

(define (truncate-msg s)
  (if (> (string-length s) max-msg-len)
    (string-append (substring s 0 (- max-msg-len 3)) "...")
    s))

; NOTE: this is an 'any, therefore slow
;(When ((?p (error ?class) ?msg))
; do (Wish ?p 'subtitled
;      (string-append "ERR[" (symbol->string ?class) "]: " (truncate-msg ?msg))))

; TODO: draw errors ourselves, so we can color them red, for example

(When ((?p (error io) ?msg))
 do (Wish ?p 'titled (string-append "ERR[IO]: " (truncate-msg ?msg))))

(When ((?p (error compile) ?msg))
 do (Wish ?p 'titled (string-append "ERR[COMPILE]: " (truncate-msg ?msg))))

(When ((?p (error runtime) ?msg))
 do (Wish ?p 'subtitled (string-append "ERR[RUNTIME]: " (truncate-msg ?msg))))
