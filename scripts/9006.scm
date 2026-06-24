; Netrunner background script
; Runs the game
; TODO: make live on the table and swappable
; TODO: swap game engine (this) with 'deckbuilding mode' tag

; NOTE:
; 28 - Sure Gamble

; Identity card projects game areas:
; Corporate has R&D, HQ and Archives, Runner nothing
; Both have a large area to play either programs/hardware or remote servers/ice
; and a card-sized area to 'play' a card (events/operations)

; Moving between areas advances the state machine of the engine

; Cards are outlined, based on MTG card dimensions, with tag in center of illustration
(When ((?p card-type ?ct)
       (?p (page points) (?ulhc ?urhc ?llhc ?lrhc)))
 do (let* ((margin (- (/ 4 5)))
           (dx (/ 9 5))
           (dy (/ 15 5)))
      (Wish ?p 'has-region-from-tag 
       `(outline ,margin ,margin ,dx ,margin ,margin ,dy ,dx ,dy))))
