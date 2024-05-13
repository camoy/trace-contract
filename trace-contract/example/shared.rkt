#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "increment.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; For two counters that are linked by a shared store. Return values from both
;; functions should contribute to the same trace.
(define (make-counter-contracts)
  (trace/c ([y integer?])
    #:global
    (values (-> y) (-> y))
    (full (y) increments?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; Two counters that share state, starting at 1.
   #:do
   (define (make-correct-counters)
     (let ([state 0])
       (define (f)
         (set! state (add1 state))
         state)
       (values f f)))

   #:do
   (define-values/contract (counter1 counter2)
     (make-counter-contracts)
     (make-correct-counters))

   (list (counter1) (counter2)
         (counter1) (counter2)
         (counter1) (counter2))
   '(1 2 3 4 5 6)

   ;; Makes two counters that share state, incorrectly. It will skip 5.
   #:do
   (define (make-wrong-counters)
     (let ([state 0])
       (define (f)
         (set! state (add1 state))
         (if (= state 5) (add1 state) state))
       (values f f)))

   #:do
   (define-values/contract (wrong-counter1 wrong-counter2)
     (make-counter-contracts)
     (make-wrong-counters))

   (list (wrong-counter1) (wrong-counter2)
         (wrong-counter1) (wrong-counter2))
   '(1 2 3 4)

   #:x
   (wrong-counter1)
   (trace-exn? '(definition wrong-counter1)
               (current-contract-region)
               "y: (1 2 3 4 6)")

   #:do
   (define-values/contract (wrong-counter3 _)
     (make-counter-contracts)
     (make-wrong-counters))

   (list (wrong-counter3) (wrong-counter3)
         (wrong-counter3) (wrong-counter3))
   '(1 2 3 4)

   #:x
   (wrong-counter3)
   (trace-exn? '(definition wrong-counter3)
               (current-contract-region)
               "y: (1 2 3 4 6)")
   ))
