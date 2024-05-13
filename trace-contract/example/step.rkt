#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Returns whether count and set behave correctly.
(define (count+set acc e)
  (match-define (list step state) acc)
  (match e
    [(list 'c x)
     (define state* (+ step state))
     (if (= x state*)
         (list step state*)
         (fail))]
    [(list 's n)
     (list n state)]))

;; Use `list/t` to tag values.
(define (make-contracts)
  (trace/c ([x integer?])
    #:global
    (values (-> (list/t 'c x))
            (-> (list/t 's x) any))
    (accumulate '(1 0) [(x) count+set])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; A correct sequence of count and set operations.
   #:do
   (define (do-count-set count set)
     (list (count)
           (count)
           (begin
             (set 2)
             (count))
           (count)
           (begin
             (set -1)
             (count))))

   ;; Sets the count's step size. Gets the next count.
   #:do
   (define (make-count+set)
     (let ([step 1]
           [state 0])
       (values (λ ()
                 (set! state (+ step state))
                 state)
               (λ (n)
                 (set! step n)))))

   #:do
   (define-values/contract (count set)
     (make-contracts)
     (make-count+set))

   (do-count-set count set)
   '(1 2 4 6 5)

   ;; Sets the count's step size. Gets the next count. But incorrectly for
   ;; negative steps.
   #:do
   (define (make-wrong-count+set)
     (let ([step 1]
           [state 0])
       (values (λ ()
                 (set! state (+ (abs step) state))
                 state)
               (λ (n)
                 (set! step n)))))

   #:do
   (define-values/contract (wrong-count wrong-set)
     (make-contracts)
     (make-wrong-count+set))

   #:x
   (do-count-set wrong-count wrong-set)
   (trace-exn? '(definition wrong-count)
               (current-contract-region)
               "produced: '(c 7)")
   ))
