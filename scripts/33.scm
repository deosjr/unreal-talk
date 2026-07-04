; rule inspector

(Wish this 'has-whiskers #t)

(define max-src-len 60)

(define (truncate-str s)
  (if (> (string-length s) max-src-len)
    (string-append (substring s 0 (- max-src-len 3)) "...")
    s))

(define (fmt datum) (format #f "~a" datum))

; entry: (src fired matches ms)
(define (stringify entry)
  (format #f "~ax  ~,2fms ~a"
          (cadr entry)
          (cadddr entry)
          (truncate-str (fmt (car entry)))))

; slowest first: the frame budget reads top-down
(define (by-time a b) (> (cadddr a) (cadddr b)))

(define (by-src a b) (string>? (fmt (car a)) (fmt (car b))))

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
 do (let* ((lines (map stringify (sort ?rules by-src)))
           (dx (- (car ?lrhc) (car ?ulhc)))
           (dy (- (cdr ?lrhc) (cdr ?ulhc)))
           (img (create-image dx dy 16))) ; 16 is 3-channel CV8U
      (draw-rectangle img 0 0 dx dy 0 0 0 -1)
      (draw-text (if (null? lines) '("(no rules)") lines)
                 img (cons 0 0) (cons dx dy))
      (draw-mat-onto-region-opaque img projection ?rotation ?ulhc ?lrhc)
      (free-image img)))

; TODO: use new region format for this
(When ((this (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (/ 8 5)) (dx (/ 50 5)) (dy (/ 50 5)))
      (Wish this 'has-region-from-tag
       `(outline ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))
      (Wish this 'has-region-from-tag-unrotated
       `(ruleslist ,margin 0 ,dx 0 ,margin ,dy ,dx ,dy))))
