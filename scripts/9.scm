; grid definition

(define mw 0)

(When ((time now ,?t))
 do (Claim-derived this this 'total-mw-production mw)
    (Wish-derived this this 'subtitled (format #f "~d MW" mw))
    (set! mw 0))

(When ((,?someone generates ,?mw))
 do (Wish-derived this ?someone 'update-mw ?mw))

(When ((,this wishes (,?someone update-mw ,?mw)))
 do (set! mw (+ mw ?mw)))
