#lang racket/base

(require pict
         "lib/main.rkt"
         "lib/trace.rkt"
         "../util/measure.rkt")

(define ITERS 3)

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 250804]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main fevents)
  (define events
    (for/list ([k (in-naturals)]
               [e (in-list fevents)])
      (indexed-future-event k e)))
  (for ([_ (in-range ITERS)])
    (pict->bitmap (timeline-pict events)))
  (done))

(define-main-module
  #:entry-point main
  #:data "data/mandelbrot.rktd")
