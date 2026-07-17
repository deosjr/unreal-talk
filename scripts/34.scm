; 8 patches in a ring
; a b c
; d x e
; f g h
(When ((this (region tag) ?r)
       (?r (region points) ?pts))
 do (Wish this '(region a up)
      `(,?pts 4 4 1 -4))
    (Wish this '(region b up)
      `(,?pts 4 4 1 0))
    (Wish this '(region c up)
      `(,?pts 4 4 1 4))
    (Wish this '(region d left)
      `(,?pts 4 4 1 0))
    (Wish this '(region e right)
      `(,?pts 4 4 1 0))
    (Wish this '(region f down)
      `(,?pts 4 4 1 -4))
    (Wish this '(region g down)
      `(,?pts 4 4 1 0))
    (Wish this '(region h down)
      `(,?pts 4 4 1 4))
)

(When ((this (region a) ?a)
       (this (region b) ?b)
       (this (region c) ?c)
       (this (region d) ?d)
       (this (region e) ?e)
       (this (region f) ?f)
       (this (region g) ?g)
       (this (region h) ?h))
 do 
    (let ((patches (list ?a ?b ?c ?e ?h ?g ?f ?d)))
      (for-each (lambda (x)
        (Wish x 'outlined 'white)
        (Claim x '(region type) 'reus)
      ) patches)

      (for-each (lambda (i)
        (let ((p (list-ref patches i))
              (left (list-ref patches
                (modulo (- i 1) 8)))
              (right (list-ref patches
                (modulo (+ i 1) 8))))
          (Claim p 'adjacent left)
          (Claim p 'adjacent right)
        )
      ) (iota 8))
    )
)

(When ((?o (region name) a)
       (?r adjacent ?o))
 do (Wish ?r 'outlined 'blue))


(When ((?o (region name) a))
 do (Wish ?o 'labeled "A"))
