#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [SLEEP natural?]
  [MIN-PLAYERS natural?]
  [MAX-PLAYERS natural?]
  [ready-time (parameter/c (and/c real? positive?))]

  [admin-player/c contract?]
  [game-observer/c contract?]
  [referee/c contract?]
  [referee-no-load/c contract?]
  [referee-load/c contract?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         trace-contract
         "../../util/measure.rkt"
         "../lib/list.rkt"
         "board.rkt"
         "game-state.rkt"
         "internal-player.rkt"
         "penguin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define SLEEP 4)
(define MIN-PLAYERS 2)
(define MAX-PLAYERS 4)

;; The number of seconds guaranteed for the first `take-turn` call.
(define ready-time (make-parameter 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `admin-player/c` and `game-observer/c`

(define admin-player/c
  (object/c
   ;; The tournament manager informs this player of the nicknames of all
   ;; paricipants.
   [start-of-tournament (->m boolean? any)]

   ;; The tournament manager informs this player of the prize winners,
   ;; a subset of those handed to start-of-tournament.
   [end-of-tournament (->m boolean? any)]))

(define game-observer/c
  (-> fishes? (-> fishes? (or/c string? (list/c turn? fishes?)) any)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol

(include "referee-trace-contract.rkti")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-referee/c`

(define (make-referee/c player*/c)
  (->i ([fishes (or/c #f fishes?)])
       (#:time-out [time-out positive?]
        #:lop [lop (fishes) (if (boolean? fishes) player*/c any/c)]
        #:size [size (list/c natural? natural?)]
        #:fixed [fixed natural?]
        #:observers [observers (listof game-observer/c)])
       [result any/c]))

(define (make-player*/c this/c state/c actions/c)
  (and/c (listof (object/c [take-turn (-> this/c state/c actions/c move/c)]))
         (property/c length (between/c MIN-PLAYERS MAX-PLAYERS))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `referee/c` and variants

(define referee/c
  (make-referee/c (make-player*/c any/c fishes? (listof turn?))))

(define referee-no-load/c
  (make-referee/c (make-fold-player*/c (λ _ #t))))

(define referee-load/c
  (make-referee/c (make-fold-player*/c protocol-check)))
