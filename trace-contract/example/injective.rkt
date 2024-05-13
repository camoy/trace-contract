#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; A function is injective if f(x) = f(y) ⇒ x = y. Equivalently, the pre-image
;; of any element of the co-domain is a singleton or empty. This checks a trace
;; of input and output pairs for injectivity.
(define (injective-folder pre-image-map xy)
  (match-define (list x y) xy)
  (if (or (not (hash-has-key? pre-image-map y))
          (equal? (hash-ref pre-image-map y) x))
      (hash-set pre-image-map y x)
      (fail)))

;; A contract for injectivity.
(define injective/c
  (trace/c ([x+y any/c])
    (->i ([x any/c])
         [y (x) (list/t x x+y)])
    (accumulate (hash) [(x+y) injective-folder])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:do (define/contract negate injective/c -)
   (negate 0) 0
   (negate 1) -1
   (negate -1) 1
   (negate 5) -5
   (negate -100) 100

   #:do (define/contract abs* injective/c abs)
   (abs* 0) 0
   (abs* 1) 1
   (abs* -5) 5
   #:x
   (abs* 5)
   (trace-exn? '(definition abs*)
               (current-contract-region)
               "'(5 5)")
   ))
