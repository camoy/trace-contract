#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(module server racket/base
  (require racket/contract
           racket/stream
           (submod "util/test.rkt"))

  (provide
   server-party
   (contract-out [add-one even-input-output/c]))

  (define server-party (current-contract-region))

  (define (add-one x)
    (add1 x))

  (define (folder _ x y)
    (or (even? x) (fail)))

  (define even-input-output/c
    (trace/c ([x integer?] [y integer?])
      (-> x y)
      (accumulate #t [(x y) folder])))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/format
           racket/pretty
           (submod ".." server))

  (define client-party (current-contract-region))

  (chk
   (add-one 0) 1
   (add-one 2) 3

   ;; Should have two blamed parties (both client and server).
   #:x
   (add-one 1)
   (trace-exn? (list server-party client-party)
               (list client-party server-party)
               "given: (values 1 2)")
   ))
