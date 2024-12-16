#lang racket

;; an interface for strategies

;; -----------------------------------------------------------------------------
(provide strategy/c% HOLD-10 BUY-NOW CHEAT)

;; -----------------------------------------------------------------------------
(require "../common/basic-constants.rkt")
(require "../common/connection.rkt")
(require "../common/map.rkt")
(require "../common/state.rkt")

;; -----------------------------------------------------------------------------
(define strategy/c%
  (class/c
   (init-field (the-game-map game-map?) (rails# natural?) (cards (listof color?)))
   (pick-destinations (->m (set/c any/c) (set/c any/c)))
   (choose-action     (->m pstate? (or/c string? action?)))))

(define HOLD-10 "Hold-10")
(define BUY-NOW "Buy-Now")
(define CHEAT   "Cheat")

;; -----------------------------------------------------------------------------
;; implemented strategies

#| Using the standard datatype pattern (see Fundamentals II, week 2): 


+-----------+                   +-----------+
| istrategy | <---------------- | astrategy |
+-----------+                   +-----------+
                                     ^
                                     |
                            +------------------+
                            |                  |
                +------------------+      +------------------+
                | hold-10-strategy |      | buy-now-strategy |
                +------------------+      +------------------+
                            ^
                            |
                +-------------------+
                | cheat-strategy    |
                +-------------------+

The filenames are the same as the class names. The `astrategy` class
uses Beta-style inheritance _and_ Java-style inheritance to set up
basic defaults 

|#

