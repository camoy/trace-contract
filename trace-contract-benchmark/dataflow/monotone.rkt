#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide analysis/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         racket/bool
         racket/class
         racket/function
         racket/include
         racket/list
         racket/match
         racket/stream
         stream-etc
         trace-contract
         "../util/measure.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(include "trace-contract.rkti")

;; Creates a contract for a static analysis.
(define (make-analysis/c transfer/c)
  (class-object/c
   (class/c)
   (object/c [f transfer/c])))

(define transfer-fold-no-load/c
  (make-transfer-fold/c
   (λ (acc self ℓ x y) #t)))

(define transfer-fold/c
  (make-transfer-fold/c transfer-fold))

(define-modifiable
  #:level base
  (define analysis/c (make-analysis/c (-> any/c any/c any/c any/c)))

  #:level noop
  (define analysis/c (make-analysis/c transfer-fold-no-load/c))

  #:level check
  (define analysis/c (make-analysis/c transfer-fold/c)))
