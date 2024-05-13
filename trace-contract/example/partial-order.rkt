#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide partial-order/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require graph
         racket/function
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Checks that a trace of inputs and outputs represents a partial order.
;; Reflexivity and transitivity imply that if there is a path from x to y then
;; x ⊑ y. The contrapos. of this is that ¬(x ⊑ y) ⇒ there is no path from x to y.
;; This is something we can check.
(define (partial-order? xyzs)
  (define g (unweighted-graph/directed null))
  (for/and ([xyz (in-stream xyzs)])
    (match-define (list x y less-than?) xyz)
    (cond
      ;; checks anti-symmetry
      [less-than?
       ;; don't add self-edges
       (when (not (equal? x y))
         (add-directed-edge! g x y))
       (dag? g)]

      ;; checks refl. and trans.
      [else
       (not (hash-ref (transitive-closure g) (list x y) (const #f)))])))

;; For checking if a comparator is a partial order.
(define partial-order/c
  (trace/c ([xyz any/c])
    (->i ([x any/c]
          [y any/c])
         [z (x y) (list/t x y xyz)])
    (full (xyz) partial-order?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract <=* partial-order/c <=)
   #:t (<=* 1 2)
   #:! #:t (<=* 2 1)
   #:t (<=* 1 1)

   #:do
   (define/contract (not-equal? x y)
     partial-order/c
     (not (equal? x y)))
   #:t (not-equal? 1 2)
   #:t (not-equal? 2 3)

   #:do
   (define/contract (≡5? x y)
     partial-order/c
     (= (modulo x 5) (modulo y 5)))
   #:! #:t (≡5? 1 2)
   #:! #:t (≡5? 2 1)
   #:t (≡5? 1 1)

   #:do
   (define/contract (dist<=5? x y)
     partial-order/c
     (<= (abs (- y x)) 5))

   ;; Violation: Reflexivity
   #:x
   (not-equal? 1 1)
   (trace-exn? '(function not-equal?)
               (current-contract-region)
               "xyz: ((1 2 #t) (2 3 #t) (1 1 #f))")

   ;; Violation: Anti-symmetry
   #:t (≡5? 0 5)
   #:x (≡5? 5 0)
   (trace-exn? '(function ≡5?)
               (current-contract-region)
               "xyz: ((1 2 #f) (2 1 #f) (1 1 #t) (0 5 #t) (5 0 #t))")

   ;; Violation: Transitivity
   #:t (dist<=5? 0 3)
   #:t (dist<=5? 3 6)
   #:x (dist<=5? 0 6)
   (trace-exn? '(function dist<=5?)
               (current-contract-region)
               "xyz: ((0 3 #t) (3 6 #t) (0 6 #f))")
   ))
