; grid definition

(define f0 50)
(define freq 50.0)
(define df 0)
(define dampingfactor 100)
(define inertiaM 5000)
(define consMW 0)
(define prodMW 0)

(When ((time now ,?t))
 do (Claim-derived this this 'total-mw-consumption consMW)
    (Claim-derived this this 'total-mw-production prodMW)
    (Wish-derived this this 'subtitled (format #f "prod/cons: ~d/~d MW" prodMW consMW))
    (calculate-frequency-deviation)
    (Claim-derived this this 'grid-frequency freq)
    (set! prodMW 0)
    (set! consMW 0))

(define (calculate-frequency-deviation)
  (let* ((powerdelta (- prodMW consMW))
         (ddf (exact->inexact (/ (- powerdelta (* dampingfactor df)) inertiaM)))
         (newdf (+ df ddf))
         (newfreq (+ f0 newdf)))
    (set! df newdf)
    (set! freq newfreq)))

(When ((,?someone generates ,?mw))
 do (Wish-derived this ?someone 'update-prod-mw ?mw))

(When ((,this wishes (,?someone update-prod-mw ,?mw)))
 do (set! prodMW (+ prodMW ?mw)))

(When ((,?someone consumes ,?mw))
 do (Wish-derived this ?someone 'update-cons-mw ?mw))

(When ((,this wishes (,?someone update-cons-mw ,?mw)))
 do (set! consMW (+ consMW ?mw)))
