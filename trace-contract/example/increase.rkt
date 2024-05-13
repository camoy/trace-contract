#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Returns whether the stream of elements increments by the associated step size.
(define (increasing-by-step? ns+xs)
  (for/fold ([state 0])
            ([n+x (in-stream ns+xs)])
    #:break (not state)
    (match-define (list n x) n+x)
    (define state* (+ state n))
    (and (= x state*) state*)))

;; For a factory that produces counters that increment by the argument given to
;; the factory. All counters share state.
(define factory/c
  (trace/c ([y integer?])
    (->i ([x integer?])
         [_ (x) (-> (list/t x y))])
    (full (y) increasing-by-step?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; Returns a counter that increments by n, but all counters share the same
   ;; state.
   #:do
   (define/contract factory
     factory/c
     (let ([state 0])
       (λ (n)
         (λ ()
           (set! state (+ n state))
           state))))

   #:do (define counter1 (factory 1))
   #:do (define counter2 (factory 2))

   (list (counter1) (counter2)
         (counter1) (counter2)
         (counter1) (counter2))
   '(1 3 4 6 7 9)

   ;; Returns a counter that increments by n, but all counters share the same
   ;; state. Except it is wrong when the state is 10 and over.
   #:do
   (define/contract wrong-factory
     factory/c
     (let ([state 0])
       (λ (n)
         (λ ()
           (set! state (+ n state))
           (if (>= state 10) (add1 state) state)))))

   #:do (define wrong-counter1 (wrong-factory 1))
   #:do (define wrong-counter2 (wrong-factory 2))

   (list (wrong-counter1) (wrong-counter2)
         (wrong-counter1) (wrong-counter2)
         (wrong-counter1) (wrong-counter2))
   '(1 3 4 6 7 9)

   #:x (wrong-counter1)
   (trace-exn? '(definition wrong-factory)
               (current-contract-region)
               "y: ((1 1) (2 3) (1 4) (2 6) (1 7) (2 9) (1 11))")
   ))
