#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define combine/c
  (trace/c ([y integer?])
    (-> any/c y)
    (combine
     (combine
      (accumulate #t [(y) (λ (acc y) (or (even? y) (fail)))]))
     (combine
      (accumulate #t [(y) (λ (acc y) (or (positive? y) (fail)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract even-values combine/c values)
   (even-values 10)  10
   #:x (even-values 3)
   "produced: 3"

   #:do (define/contract positive-values combine/c values)
   (positive-values 10)  10
   #:x (positive-values -4)
   "produced: -4"
   ))
