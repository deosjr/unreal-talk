; Surface 'has-error facts as subtitles under the offending page.
;
; has-error is asserted by realtalk.scm's error handlers:
;   - rule body throws during fixpoint (derived fact, auto-clears next
;     fixpoint if the body stops throwing)
;   - save-page's eval-string fails (EDB fact, cleared on next good save)
;   - page-moved-onto-table's init throws (EDB fact, cleared on next
;     good save or when the page leaves the table)
;
; Reusing 9001's 'subtitled wish means error rendering goes through the
; same path as ordinary labels, and the subtitle vanishes for free when
; has-error retracts. Full message stays in stderr; this is just the
; on-table glance value.

(define max-msg-len 60)

(define (truncate-msg s)
  (if (> (string-length s) max-msg-len)
    (string-append (substring s 0 (- max-msg-len 3)) "...")
    s))

(When ((?p has-error ?msg))
 do (Wish ?p 'subtitled (string-append "ERR: " (truncate-msg ?msg))))
