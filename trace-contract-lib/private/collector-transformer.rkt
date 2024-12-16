#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide map/t
         list/t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         "collector-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformers

;; Applies a function before the value is added to the trace.
(define (map/t f cctc)
  (define old-mapper (collector-contract-mapper cctc))
  (define mapper* (if old-mapper (compose f old-mapper) f))
  (define name* `(map/t ,(object-name f) ,(contract-name cctc)))
  (if (chaperone-collector-contract? cctc)
      (struct-copy chaperone-collector-contract cctc
                   [name #:parent collector-contract name*]
                   [mapper #:parent collector-contract mapper*])
      (struct-copy impersonator-collector-contract cctc
                   [name #:parent collector-contract name*]
                   [mapper #:parent collector-contract mapper*])))

;; Collector transformer that produces lists.
(define (list/t . xs)
  (define collector (findf collector-contract? xs))
  (map/t (λ (y)
           (for/list ([x (in-list xs)])
             (if (collector-contract? x) y x)))
         collector))
