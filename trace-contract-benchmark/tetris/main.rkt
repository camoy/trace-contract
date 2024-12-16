#lang racket

(require pict
         "aux.rkt"
         "world.rkt"
         "bset.rkt"
         "data.rkt"
         "visual.rkt"
         "../util/measure.rkt")

(define (world0)
  (world (list-pick-random tetras) empty))

(define (game-over? w)
  (blocks-overflow? (world-blocks w)))

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    (pict->bitmap (world->image w))
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (game-over? w)
       w])))

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 132377]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main raw)
  (define w0 (world0))
  (if (list? raw)
      (replay w0 raw)
      (error "bad input"))
  (done))

(define-main-module
  #:entry-point main
  #:data "base/tetris-hist.rktd")
