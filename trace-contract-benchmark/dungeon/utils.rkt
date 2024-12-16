#lang racket/base

(require racket/contract)
(provide
 article
 random-from
 (contract-out
  [random random/c]
  [random-between random-between/c]
  [reset! reset!/c]))

(require
 contract-etc
 racket/include
 trace-contract
 "../util/measure.rkt"
 (only-in racket/list first)
 (only-in racket/file file->value))

;; =============================================================================

(define orig '(2 10 24 3 0 2 10 45 2 2 2 2 49 3 1 5 1 0 0 2 1 0 2 1 0 0 2 2 5 0 0 0 3 0 1 2 0 3 0 0 2 2 0 2 2 0 0 3 0 0 2 0 3 1 0 2 0 0 1 1 0 2 0 0 3 0 0 1 2 0 3 1 0 2 0 0 0 1 3 1 1 0 1 2 0 3 2 0 1 2 0 1 1 0 2 2 0 1 1 0 2 2 0 0 0 2 1 0 0 0 0 3 4 0 0 2 1 0 2 1 0 3 1 0 1 0 0 1 0 0 1 2 0 1 0 0 2 2 0 2 2 0 3 1 0 1 0 0 1 1 0 2 1 0 3 2 0 3 0 0 2 2 0 0 0 3 4 2 0 3 0 0 3 1 0 0 3 0 4 0 0 2 0 0 2 2 0 2 1 0 0 0 3 6 1 0 3 0 0 0 2 1 3 0 0 3 1 0 1 1 0 2 0 0 3 2 0 2 1 0 1 2 0 0 3 0 2 2 0 2 2 0 2 2 0 1 1 0 3 1 0 2 1 0 1 2 0 0 2 0 3 1 0 1 1 0 2 2 0 2 2 0 1 5 3 3 2 1))
(define r* (box orig))

(define (reset!)
  (set-box! r* orig))

(define (random n)
  (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

;; CCM: Make sure we use the uncontracted `random` internally.
(define random-between
  (let ([random random])
    (λ (min max)
      (+ min (random (- max min))))))

(define (random-from l)
  (first (shuffle l)))

(define (shuffle l)
  (reverse l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(include "trace-contract.rkti")

(define (max-calls-fold-no-load/c n)
  (make-max-calls-fold/c (λ (acc x) #t) n))

(define (max-calls-fold/c n)
  (make-max-calls-fold/c (make-max-calls-folder n) n))

(define-modifiable
  #:level base
  (define random/c (apply/c [integer? 1]))
  (define random-between/c (apply/c [integer? 1]))
  (define reset!/c (apply/c [symbol? 'reset]))

  #:level noop
  (define-values (random/c random-between/c reset!/c)
    (max-calls-fold-no-load/c (length orig)))

  #:level check
  (define-values (random/c random-between/c reset!/c)
    (max-calls-fold/c (length orig))))
