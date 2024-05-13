#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Folding procedure for `counter/c`.
(define (counter-fold acc y)
  (if (= y acc) (add1 acc) (fail)))

;; For a zero-argument function whose return value is incremented upon each
;; invocation.
(define counter/c
  (trace/c ([y integer?])
    (-> y)
    (accumulate 1 [(y) counter-fold])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; A correct counter that starts from 1.
   #:do
   (define/contract counter
     counter/c
     (let ([state 0])
       (λ ()
         (set! state (add1 state))
         state)))

   (list (counter) (counter) (counter))
   '(1 2 3)

   ;; An incorrect counter that will skip 3 and go directly to 4.
   #:do
   (define/contract wrong-counter
     counter/c
     (let ([state 0])
       (λ ()
         (set! state (add1 state))
         (if (= state 3) (add1 state) state))))

   (list (wrong-counter) (wrong-counter))
   '(1 2)

   #:x (wrong-counter)
   (trace-exn? '(definition wrong-counter)
               (current-contract-region)
               "produced: 4")
   ))
