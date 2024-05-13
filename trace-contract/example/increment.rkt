#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide increments?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Returns whether the stream is a prefix of the positive integers.
(define (increments? xs)
  (define n (stream-length xs))
  (equal? (stream->list xs)
          (range 1 (+ n 1))))

;; For a zero-argument function whose return value is incremented upon each
;; invocation.
(define counter/c
  (trace/c ([y integer?])
    (-> y)
    (full (y) increments?)))

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
               "y: (1 2 4)")
   ))
