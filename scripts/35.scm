(Wish this 'titled "biotica")

(define (id name) (list 'bioticum name))

; todo: we cannot define a procedure to simplify
; but we can use a macro?
(Claim (id 'blueberry) '(bioticum name) "Blueberry")
(Claim (id 'blueberry) '(bioticum type) 'plant)
(Claim (id 'blueberry) '(bioticum yield) '(10 0 0))

; todo: this fires for all adjacent minerals
; fix: use Collect?
(When ((?r (region type) reus)
       (?r (reus bioticum) ,(id 'blueberry))
       (?r adjacent ?o)
       (?o (reus bioticum) ?b)
       (?b (bioticum type) mineral))
 do (Claim ?r '(yield bonus) (cons (gensym) '(10 0 0))))

(Claim (id 'moss-agate) '(bioticum name) "Moss Agate")
(Claim (id 'moss-agate) '(bioticum type) 'mineral)
(Claim (id 'moss-agate) '(bioticum yield) '(0 15 0))

(When ((?o (region name) c))
 do (Claim ?o '(reus bioticum) (id 'blueberry)))

(When ((?o (region name) e))
 do (Claim ?o '(reus bioticum) (id 'moss-agate)))
(When ((?o (region name) g))
 do (Claim ?o '(reus bioticum) (id 'moss-agate)))

; total yield = (base + bonus) * multiplier
; yield is calculated per patch, not per bioticum
; we can claim multiple facts about base/bonus/multiplier
; then collect them and add them into a total yield
; NOTE: claiming 2 identical bonuses is possible but they have to be
; unique facts. For now, I use gensym to hack around this
; TODO: at some point we could cache this using Remember
; only to be recalculated when a bioticum is placed/removed
(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum yield) ?yield))
 do (Claim ?r '(yield base) ?yield)
    (Claim ?r '(yield bonus) (cons (gensym) '(0 0 0))))

(Collect ((?r (region type) reus)
          (?r (yield bonus) ?yield))
  emit (cons ?r (cdr ?yield)) ; yield has gensym as car
  as   yields
  do (let ((m (make-hash-table)))
       (for-each (lambda (x)
         (let* ((r (car x)) (yields (cdr x))
                (prev (hash-ref m r '(0 0 0))))
           (hash-set! m r (map + prev yields)))
       ) yields)
       (hash-for-each (lambda (k v)
         (Claim k '(yield bonus total) v)
       ) m)))

(When ((?r (yield base) ?base)
       (?r (yield bonus total) ?bonus))
 do (Claim ?r '(yield total) (map + ?base ?bonus)))

(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum name) ?name)
       (?r (yield total) (?f ?g ?s)))
 do (let ((text (format #f "~a: ~a ~a ~a" ?name ?f ?g ?s)))
      (Wish ?r 'labeled text)))
