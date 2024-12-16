#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/contract
         "../util/measure.rkt"
         "constant-propagation.rkt"
         "lfp.rkt"
         "monotone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 735]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main prog)
  (define cp (new constant-propagation% [stmt* prog]))
  (define lfp (new LFP% [framework cp]))
  (send lfp entry-facts)
  (send lfp exit-facts)
  (done))

(define-main-module
  #:entry-point main
  #:data "data/dataflow-hist.rktd")

#;(define eg
    (term
     (seq {(:= x 5) ℓ1}
          (while {(= 1 1) ℓ2}
            {(:= x 7) ℓ3}))))
