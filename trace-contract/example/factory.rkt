#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "increment.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; For a function that returns a counter. A "counter factory." Each counter
;; returned by the factory is treated independently.
(define factory/c
  (-> (trace/c ([y integer?])
        (-> y)
        (full (y) increments?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; Factory returns a counter with fresh state.
   #:do
   (define/contract (factory)
     factory/c
     (define state 0)
     (λ ()
       (set! state (add1 state))
       state))

   #:do (define counter1 (factory))
   #:do (define counter2 (factory))

   (list (counter1) (counter2)
         (counter1) (counter2)
         (counter1) (counter2))
   '(1 1 2 2 3 3)

   ;; Factory returns a counter with fresh state, incorrectly. It skips over 3.
   #:do
   (define/contract (wrong-factory)
     factory/c
     (define state 0)
     (λ ()
       (set! state (add1 state))
       (if (= state 3) (add1 state) state)))

   #:do (define wrong-counter1 (wrong-factory))
   #:do (define wrong-counter2 (wrong-factory))

   (list (wrong-counter1) (wrong-counter1))
   '(1 2)

   #:x
   (wrong-counter1)
   (trace-exn? '(function wrong-factory)
               (current-contract-region)
               "y: (1 2 4)")

   (list (wrong-counter2) (wrong-counter2))
   '(1 2)
   ))
