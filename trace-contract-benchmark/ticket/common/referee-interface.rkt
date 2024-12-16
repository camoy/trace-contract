#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide manager-player/c
         referee/c
         referee-no-load/c
         referee-load/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         trace-contract
         "../../util/measure.rkt"
         "../lib/list.rkt"
         "basic-constants.rkt"
         "connection.rkt"
         "map.rkt"
         "state.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `manager-player/c`

(define manager-player/c
  (object/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol

(include "referee-trace-contract.rkti")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-referee/c`

(define (make-referee/c player*/c)
  (->* (player*/c game-map?)
       (#:cards (listof color?)
        #:shuffle (-> (listof destination/c) (listof destination/c)))
       (list/c (listof player*/c) player*/c)))

(define (make-player*/c this/c)
  (listof
   (object/c
    ;; Grant the player the right to take a turn.
    [play (-> this/c pstate? (or/c action? MORE))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `referee/c` and variants

(define referee/c (make-referee/c (make-player*/c any/c)))

(define referee-no-load/c
  (make-referee/c (make-fold-player*/c (λ _ #t))))

(define referee-load/c
  (make-referee/c (make-fold-player*/c protocol-check)))
