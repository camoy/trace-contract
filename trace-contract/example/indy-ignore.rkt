#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(module contract-party racket/base
  (provide should-ignore/c)

  (require racket/contract
           racket/stream
           (submod "util/test.rkt"))

  (define (ignore-ok? x)
    (< (stream-length x) 2))

  ;; Ignore indy when a collector is used directly as a dependent argument.
  (define should-ignore/c
    (trace/c ([x integer?])
      (->i ([arg x])
           [result (arg) any/c])
      (full (x) ignore-ok?)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." contract-party))

  (chk
   ;; Should ignore the `1` going to indy.
   #:do (define/contract g-ignore should-ignore/c values)
   (g-ignore 1) 1
   ))
