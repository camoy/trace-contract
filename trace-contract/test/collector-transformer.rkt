#lang racket/base

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
