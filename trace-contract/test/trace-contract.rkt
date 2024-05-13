#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (require (submod "decl.rkt" example)
           (submod "clause.rkt" example))

  (define pos/c
    (make-trace-contract
     '(trace/c ((x integer?)) (-> any/c x) (accumulate 1 ((x) positive-folder)))
     (list x)
     #f
     (list (λ (x)
             (clause-register! (positive-clause x))
             (-> any/c x)))
     'indy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." example))

  (chk
   #:t (chaperone-contract? pos/c)
   #:t (impersonator-contract?
        (make-trace-contract 'trace/c
                             '()
                             #f
                             (list (λ () (parametric->/c (A) (-> A A))))
                             #f))

   (contract-name pos/c)
   '(trace/c ([x integer?])
      (-> any/c x)
      (accumulate 1 [(x) positive-folder]))

   #:do (define/contract (f x) pos/c x)
   (f 10)  10
   #:x (f -20)
   "accumulator: 11"
   ))
