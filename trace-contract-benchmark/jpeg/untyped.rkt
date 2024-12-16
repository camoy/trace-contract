#lang racket/base

(require automata/machine
         automata/re
         automata/re-ext
         racket/contract
         racket/include
         racket/list
         racket/match
         racket/stream
         stream-etc
         trace-contract
         "../util/measure.rkt")

(provide
 (contract-out
  [open-output-bytes open-output-bytes/c]
  [write-byte write-byte/c]
  [close-output-port close-output-port/c])
 assert)

(define (assert v p)
  (if (p v) v (raise-user-error 'assert "~a" (object-name p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(include "trace-contract.rkti")

(define-modifiable
  #:level base
  (define open-output-bytes/c (-> output-port?))
  (define write-byte/c (-> byte? output-port? any))
  (define close-output-port/c (-> output-port? any))

  #:level noop
  (define-values (open-output-bytes/c write-byte/c close-output-port/c)
    (make-port-fold/c (λ (acc x) #t)))

  #:level check
  (define-values (open-output-bytes/c write-byte/c close-output-port/c)
    (make-port-fold/c port-folder)))
