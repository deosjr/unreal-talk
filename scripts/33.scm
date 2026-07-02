; rule inspector (print tag 33 from tagStandard41h12)
;
; Point this page at another page to see that page's rules: how often
; each fired (body invocations), how many join derivations it produced,
; and how long its joins took — all from the previous frame, rendered as
; a list beside this tag. Slowest rule on top: this page is a profiler.
;
; Everything it reads is ordinary facts. (?p rules ?r) and
; (?r (rule source) ...) are claimed by the inspected page itself when
; its Whens register; the per-frame stats are engine-claimed by the
; engine at frame start (one frame stale, like any Collect result).
;
; NOTE: the region is a CONDITION of the drawing rule, not something to
; query for inside a body. Region facts are derived claims (9004), so
; they only exist from iteration ~3 of each fixpoint onward — a query in
; a body that runs earlier would reliably see nothing. Conditions fire
; the rule exactly when the fact exists; the Collect hands its aggregate
; over through a page-local variable (reduce runs at iteration 1, before
; the drawing rule can fire, so the hand-off is always ordered).

(Wish this 'has-whiskers #t)

(define max-src-len 60)

(define (truncate-str s)
  (if (> (string-length s) max-src-len)
    (string-append (substring s 0 (- max-src-len 3)) "...")
    s))

; entry: (src fired matches ms)
(define (stringify entry)
  (format #f "~ax ~,2fms ~a"
          (cadr entry)
          (cadddr entry)
          (truncate-str (format #f "~a" (car entry)))))

; slowest first: the frame budget reads top-down
(define (by-time a b) (> (cadddr a) (cadddr b)))

(define last-rules '())

(Collect ((this points-at ?p)
          (?p rules ?r)
          (?r (rule source) ?src)
          (?r (rule fired) ?f)
          (?r (rule matches) ?m)
          (?r (rule time-ms) ?ms))
  emit (list ?src ?f ?m ?ms)
  as   rules
  do (Claim this 'ruleslist rules))

(When ((this ruleslist ?rules)
       (this (region ruleslist) (?rotation ?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((lines (map stringify (sort ?rules by-time)))
           (dx (- (car ?lrhc) (car ?ulhc)))
           (dy (- (cdr ?lrhc) (cdr ?ulhc)))
           (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
;(display ?p)
      (draw-rectangle img 0 0 dx dy 0 0 0 -1)
      (draw-text (if (null? lines) '("(no rules)") lines)
                 img (cons 0 0) (cons dx dy))
      (draw-mat-onto-region-opaque img projection ?rotation ?ulhc ?lrhc)
      (free-image img)))

(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish this 'has-region-from-tag
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(ruleslist ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))
