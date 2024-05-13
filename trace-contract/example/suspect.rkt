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
   (contract-out [double even-input-output/c]))

  (define server-party (current-contract-region))

  (define (double x)
    (* 2 x))

  (define (folder acc t)
    (cond
      [(or (eq? acc 'RESET) (even? t)) t]
      [else (fail #:reset 'RESET)]))

  (define even-input-output/c
    (trace/c ([t integer?])
      (-> t t)
      (track setof-suspect
        (accumulate 1
         [(t) folder]))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/format
           racket/pretty
           (submod ".." server))

  (define client-party (current-contract-region))

  ;; Force the suspect set string to be on one line so we can compare.
  (chk
   (double 0) 0
   (double 2) 4

   ;; Should have two suspects (both client and server).
   #:x
   (double 1)
   (trace-exn? client-party
               server-party
               (~a server-party)
               (~a client-party))

   ;; Reset should cause it always to pass.
   (double 1) 2
   ))
