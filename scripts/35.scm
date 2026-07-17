(Wish this 'titled "biotica")

(define (id name) (list 'bioticum name))

; hack: the gensym is there to enforce uniqueness
; in case we emit two otherwise identical bonus yields from different sources
(define (yield food gold science)
  (list (gensym) food gold science))

(When ((?someone wishes (?b bioticum (?name ?type ?yield ?tags))))
 do (let ((bid (id ?b)))
      (Claim bid '(bioticum name) ?name)
      (Claim bid '(bioticum type) ?type)
      (Claim bid '(bioticum yield) ?yield)))

; this way we dedupe multiple adjacencies!
(When ((?r (region type) reus)
       (?r adjacent ?o)
       (?o (reus bioticum) ?b)
       (?b (bioticum type) ?type))
 do (Claim ?r '(adjacent type) ?type))

(When ((?someone wishes (?b (symbiosis adjacent) (?type ?f ?g ?s)))
       (?r (reus bioticum) (bioticum ?b))
       (?r (adjacent type) ?type))
 do (Claim ?r '(yield bonus) (yield ?f ?g ?s)))

(Wish 'ash 'bioticum '("Ash" plant (0 0 15) (construction tree)))

(Wish 'blueberry 'bioticum '("Blueberry" plant (10 0 0) (fruit undergrowth)))
(Wish 'blueberry '(symbiosis adjacent) '(mineral 10 0 0))

; this was Collect to do at-least-one, but thats not needed
;(Collect ((?r (region type) reus)
          ;(?r (reus bioticum) ,(id 'blueberry))
          ;(?r adjacent ?o)
          ;(?o (reus bioticum) ?b)
          ;(?b (bioticum type) mineral))
  ;emit ?r 
  ;as   blueberries
  ;do (for-each (lambda (r)
       ;(Claim r '(yield bonus) (yield 10 0 0))
     ;) (delete-duplicates blueberries)))

(Wish 'rabbit 'bioticum '("Rabbit" animal (15 0 0) (domesticable fur dark-offering herbivore critter)))

(Wish 'stoat 'bioticum '("Stoat" animal (0 5 0) (fur critter predator)))
; todo: base +1 threat, +20 if adjacent to at least one other Critter

(Wish 'moss-agate 'bioticum '("Moss Agate" mineral (0 15 0) (gem)))
(Wish 'moss-agate '(symbiosis adjacent) '(plant 0 5 0))

(Wish 'mossy-stone 'bioticum '("Mossy Stone" mineral (0 0 20) (undergrowth element)))
; todo: base +1 threat

(When ((?o (region name) c))
 do (Claim ?o '(reus bioticum) (id 'blueberry)))

(When ((?o (region name) e))
 do (Claim ?o '(reus bioticum) (id 'moss-agate)))
(When ((?o (region name) g))
 do (Claim ?o '(reus bioticum) (id 'rabbit)))

; total yield = (base + bonus) * multiplier
; yield is calculated per patch, not per bioticum
; we can claim multiple facts about base/bonus/multiplier
; then collect them and add them into a total yield
; TODO: at some point we could cache this using Remember
; only to be recalculated when a bioticum is placed/removed
(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum yield) ?yield))
 do (Claim ?r '(yield base) ?yield)
    (Claim ?r '(yield bonus) (yield 0 0 0)))

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

; todo: base can change, so will need same trick as bonus yield
; same for multiplier!
(When ((?r (yield base) ?base)
       (?r (yield bonus total) ?bonus))
 do (let* ((b (map + ?base ?bonus))
           (mult 1.0) ; todo
           (total (map (lambda (x) (* x mult)) b)))
     (Claim ?r '(yield total) total)))

(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum name) ?name)
       (?r (yield total) (?f ?g ?s)))
 do (let ((text (format #f "~a: ~a ~a ~a" ?name ?f ?g ?s)))
      (Wish ?r 'labeled text)))
