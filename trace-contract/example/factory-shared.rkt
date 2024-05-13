#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "increment.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; For a factory where all the counters are shared. There is only one trace here
;; that collects all results.
(define factory/c
  (trace/c ([y integer?])
    (-> (-> y))
    (full (y) increments?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; Returns a counter that increments up, but all counters share state.
   #:do
   (define/contract factory
     factory/c
     (let ([state 0])
       (λ ()
         (λ ()
           (set! state (add1 state))
           state))))

   #:do (define counter1 (factory))
   #:do (define counter2 (factory))

   (list (counter1) (counter2)
         (counter1) (counter2)
         (counter1) (counter2))
   '(1 2 3 4 5 6)

  ;; Returns a counter that increments up, but all counters share state,
  ;; but incorrectly. It skips 5.
  #:do
  (define/contract wrong-factory
    factory/c
    (let ([state 0])
      (λ ()
        (λ ()
          (set! state (add1 state))
          (if (= state 5) (add1 state) state)))))

   #:do (define wrong-counter1 (wrong-factory))
   #:do (define wrong-counter2 (wrong-factory))

   (list (wrong-counter1) (wrong-counter2)
         (wrong-counter1) (wrong-counter2))
   '(1 2 3 4)

   #:x
   (wrong-counter1)
   (trace-exn? '(definition wrong-factory)
               (current-contract-region)
               "y: (1 2 3 4 6)")
   ))
