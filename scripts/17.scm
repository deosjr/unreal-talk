; consumes MW variable over time
; todo: t is in millis and jumps per ~100 or so, making this look weird
; but thats mostly fine, we just want some random variation

(When ((time now (,?seconds . ,?millis))) do
 (Claim-derived this this 'consumes (inexact->exact (round (+ 300 (* 20 (sin (* 0.1 ?millis))))))))
