; grid definition

(define consMW 0)
(define prodMW 0)

(When ((time now ,?t))
 do (Claim-derived this this 'total-mw-consumption consMW)
    (Claim-derived this this 'total-mw-production prodMW)
    (Wish-derived this this 'subtitled (format #f "prod/cons: ~d/~d MW" prodMW consMW))
    (set! prodMW 0)
    (set! consMW 0))

(When ((,?someone generates ,?mw))
 do (Wish-derived this ?someone 'update-prod-mw ?mw))

(When ((,this wishes (,?someone update-prod-mw ,?mw)))
 do (set! prodMW (+ prodMW ?mw)))

(When ((,?someone consumes ,?mw))
 do (Wish-derived this ?someone 'update-cons-mw ?mw))

(When ((,this wishes (,?someone update-cons-mw ,?mw)))
 do (set! consMW (+ consMW ?mw)))
