#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (folder acc _)
  (if (< acc 2) (add1 acc) (fail)))

;; Call the given function at most twice.
(define call-at-most-twice
  (trace/c ([t any/c])
    #:global
    (-> t any)
    (accumulate 0 [(t) folder])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define/contract f call-at-most-twice values)
  (define/contract g call-at-most-twice values)

  (chk
   (f 1)  1
   (g 2)  2

   #:x
   (f 3)
   (trace-exn? (current-contract-region)
               '(definition f)
               "given: 3")
   ))
