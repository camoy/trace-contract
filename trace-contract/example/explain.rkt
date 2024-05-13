#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "increment.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (counter-ok? acc y #:blame blm)
  (cond
    [(not acc) y]
    [(= (add1 acc) y) y]
    [else
     (fail #:explain
           (λ ()
             (raise-blame-error
              blm y
              '(expected: "values should increment" given: "~a (last ~a)")
              y acc)))]))

(define counter/c
  (trace/c ([y integer?])
    (-> y any)
    (accumulate #f [(y) counter-ok?])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract values* counter/c values)

   (list (values* 1) (values* 2))
   '(1 2)

   #:x
   (values* 4)
   "given: 4 (last 2)"
   ))
