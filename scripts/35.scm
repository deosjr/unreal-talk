(Wish this 'titled "biotica")

(define (id name) (list 'bioticum name))

(Claim (id 'blueberry) '(bioticum name) "Blueberry")
(Claim (id 'blueberry) '(bioticum type) 'plant)
(Claim (id 'blueberry) '(bioticum yield) '(10 0 0))

; todo: this fires for all adjacent minerals
(When ((?r (region type) reus)
       (?r (reus bioticum) ,(id 'blueberry))
       (?r adjacent ?o)
       (?o (reus bioticum) ?b)
       (?b (bioticum type) mineral))
 do (Claim ?r '(yield extra) '(10 0 0)))

(Claim (id 'moss-agate) '(bioticum name) "Moss Agate")
(Claim (id 'moss-agate) '(bioticum type) 'mineral)
(Claim (id 'moss-agate) '(bioticum yield) '(0 15 0))

(When ((?o (region name) c))
 do (Claim ?o '(reus bioticum) (id 'blueberry)))

(When ((?o (region name) e))
 do (Claim ?o '(reus bioticum) (id 'moss-agate)))

(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum yield) ?yield))
 do (Claim ?r '(yield base) ?yield))

(When ((?r (region type) reus)
       (?r (reus bioticum) ?b)
       (?b (bioticum name) ?name)
       (?r (yield base) (?f ?g ?s)))
 do (let ((text (format #f "~a: ~a ~a ~a" ?name ?f ?g ?s)))
      (Wish ?r 'labeled text)))