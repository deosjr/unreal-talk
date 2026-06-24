; Netrunner - Sure Gamble
(Claim this 'cost 5)
(Claim this 'card-type 'event)
(Claim this 'identity 'neutral)
(Claim this 'influence 0)

(Claim this 'card-text "Gain 9 credits")

(When ((?player 'played this)
       (?player 'credits ?credits)) do
  (Wish ?player 'update-credits (+ ?credits 9)))
