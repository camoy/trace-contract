#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide player/c
         player-no-load/c
         player-load/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require automata/machine
         automata/nfa
         trace-contract
         "../../util/measure.rkt"
         "basic-constants.rkt"
         "connection.rkt"
         "map.rkt"
         "state.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol

(include "player-trace-contract.rkti")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-player/c` and variants

(define (make-player/c setup/c pick/c play/c more/c win/c)
  (object/c
   ;; Hand the player the map for the game, a number of rails, and some cards.
   [setup (-> setup/c game-map? natural? (listof color?) any/c)]

   ;; Ask the player to pick some destinations and to return the remainder.
   [pick  (-> pick/c (set/c destination/c) (set/c destination/c))]

   ;; Grant the player the right to take a turn.
   [play  (-> any/c pstate? (and/c (or/c MORE action?) play/c))]

   ;; If the preceding call to `play` returned `MORE`, call `more` to hand out
   ;; more cards.
   [more  (-> more/c (listof color?) any/c)]

   ;; Inform the player whether it won (#t) or lost (#f) the game.
   [win   (-> any/c (and/c boolean? win/c) any/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `player/c` and variants

(define player/c (make-player/c any/c any/c any/c any/c any/c))
(define player-no-load/c (make-fold-player/c (λ _ #t)))
(define player-load/c (make-fold-player/c protocol-check))
