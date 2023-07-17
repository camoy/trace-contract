#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 ;; `collector-contract.rkt`
 (contract-out
  [collector-contract? predicate/c])

 ;; `collector-transformer.rkt`
 (contract-out
  [map/t (-> (-> any/c any) collector-contract? collector-contract?)]
  [list/t (->* ()
               #:rest (and/c list? (exactly-one/c collector-contract?))
               collector-contract?)])

 ;; `fail.rkt`
 (contract-out
  [fail? predicate/c]
  [rename make-fail fail (->* () (#:reset any/c #:explain (-> any)) fail?)])

 ;; `full-clause.rkt`
 full
 (contract-out
  [trace? predicate/c]
  [trace-merge (-> trace? ... trace?)])

 ;; `logger.rkt`
 (contract-out
  [trace-contract-logger logger?])

 ;; `trace-contract.rkt`
 (contract-out
  [trace-contract? predicate/c])

 ;; `trace-contract-macro.rkt`
 (for-syntax trace-clause-macro
             trace-clause-expand
             trace-clause-macro?)
 trace/c
 accumulate
 combine

 ;; `track-clause.rkt`
 track
 (contract-out
  [suspect<%> interface?]
  [setof-suspect (is-a?/c suspect<%>)]
  [listof-suspect (is-a?/c suspect<%>)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         "private/collector-contract.rkt"
         "private/collector-transformer.rkt"
         "private/fail.rkt"
         "private/full-clause.rkt"
         "private/logger.rkt"
         "private/trace-contract.rkt"
         "private/trace-contract-macro.rkt"
         "private/track-clause.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Ensures that `xs` has exactly only element that is `ok?`.
(define ((exactly-one/c ok?) xs)
  (define num-oks
    (for/sum ([x (in-list xs)]
              #:when (ok? x))
      1))
  (= num-oks 1))
