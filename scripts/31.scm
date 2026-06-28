; wormhole in

(When ((this (page points) ?pts))
 do (Wish this '(region outline down) `(,?pts 2 2 1 0)))

(When ((this (region outline) ?r) 
       (?p points-at ?r))
 do (Claim this 'wormhole ?p))
