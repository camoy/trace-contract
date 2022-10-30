#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide map/t
         list/t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "collector-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformers

;; Applies a function before the value is added to the trace.
(define (map/t f cctc)
  (define old-mapper (collector-contract-mapper cctc))
  (define mapper* (if old-mapper (compose f old-mapper) f))
  (if (chaperone-collector-contract? cctc)
      (struct-copy chaperone-collector-contract cctc
                   [mapper #:parent collector-contract mapper*])
      (struct-copy impersonator-collector-contract cctc
                   [mapper #:parent collector-contract mapper*])))

;; Collector transformer that produces lists.
(define (list/t . xs)
  (define collector (findf collector-contract? xs))
  (map/t (λ (y)
           (for/list ([x (in-list xs)])
             (if (collector-contract? x) y x)))
         collector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/contract
           (submod "collector-contract.rkt" examples))

  (chk
   #:do (define add1-collector (map/t add1 empty-collector))
   (contract-name add1-collector)  '(map/t add1 x)
   ))
