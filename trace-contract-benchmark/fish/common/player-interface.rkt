#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide player/c
         player-no-load/c
         player-load/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         "../../util/measure.rkt"
         "board.rkt"
         "game-state.rkt"
         "penguin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-player/c` and variants

(include "player-trace-contract.rkti")

(define (make-player/c playing-as/c playing-with/c initial/c take-turn/c)
  (object/c
   ;; The referee informs this player of the assigned color.
   [playing-as (-> playing-as/c penguin-color/c any)]

   ;; The referee informs this player of the colors of all players (and
   ;; implicitly their order).
   [playing-with (-> playing-with/c
                     (listof penguin-color/c)
                     any)]

   ;; This player places one an penguin per set-up initial turn.
   [initial (-> initial/c fishes? posn/c)]

   ;; This player moves one penguin per turn.
   ;; Assume: This player's color is the first in the player part of state
   ;; the actions represent what other players have done (in order) since the
   ;; last time `this` player took a turn if any; an empty list says "use the
   ;; state to make your next move."
   [take-turn (-> take-turn/c
                  (and/c fishes? not-skipped?)
                  (listof turn?)
                  move/c)]))

#; {State -> Boolean}
(define (not-skipped? f)
  (let ([a* (all-possible-actions f)])
    (and (cons? a*) (not (skip? (first a*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `player/c` and variants

(define player/c (make-player/c any/c any/c any/c any/c))
(define player-no-load/c (make-fold-player/c (λ _ #t)))
(define player-load/c (make-fold-player/c protocol-check))
