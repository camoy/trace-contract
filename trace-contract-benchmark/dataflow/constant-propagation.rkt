#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [constant-propagation% analysis/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/list
         racket/set
         redex-etc
         redex/reduction-semantics
         "../util/measure.rkt"
         "interface.rkt"
         "monotone.rkt"
         "order.rkt"
         "reduction.rkt"
         "while-class.rkt"
         "while.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define constant-propagation%
  (class* while% (monotone-framework<%>)
    (init stmt*)
    (inherit flow init blocks)
    (super-new)

    (define current-stmt* stmt*)
    (define var* (variables stmt*))
    (define blocks* (blocks stmt*))
    (define range (new bounded-discrete%))
    (define range-⊤ (send range ⊤))
    (define lattice (new pointwise-function-space% [domain var*] [range range]))
    (define ⊤ (send lattice ⊤))
    (define ⊥ (send lattice ⊥))

    (define (^op ^σ aop ^a1 ^a2)
      (cond
        [(equal? ^a1 ⊥) ⊥]
        [(equal? ^a2 ⊥) ⊥]
        [(and (integer? ^a1) (integer? ^a2)) (term (δ (,aop ,^a1 ,^a2)))]
        [else ⊤]))

    (define (acp aexp ^σ)
      (match-term
       While aexp
       [X (if (equal? ^σ ⊥) ⊥ (hash-ref ^σ (term X)))]
       [integer (if (equal? ^σ ⊥) ⊥ (term integer))]
       [(AO₂ A_1 A_2)
        (^op ^σ
             (term AO₂)
             (acp (term A_1) ^σ)
             (acp (term A_2) ^σ))]))

    (define/public (L) lattice)
    (define/public (F) (flow current-stmt*))
    (define/public (E) (set (init current-stmt*)))
    (define/public (ι) ⊤)
    (define/public (f ℓ ^σ)
      (match-term
       While (label->block ℓ blocks*)
       [{(:= X A) _}
        (if (equal? ^σ ⊥) ⊥ (hash-set ^σ (term X) (acp (term A) ^σ)))]
       [{skip _} ^σ]
       #;[{B _} (if (equal? ^σ ⊥) ⊤ ⊥)] ; bad
       [{B _} ^σ] ; good
       ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#;(module+ test
  (require chk)

  (define-term prog
    (seq
     {(:= x 1) ℓ1}
     {(:= y 2) ℓ2}
     {(:= z (+ (* x 3) y)) ℓ3}))

  (define cp (new constant-propagation% [stmt* (term prog)]))
  (define-values (⊤ ⊥)
    (values (send+ cp (L) (⊤))
            (send+ cp (L) (⊥))))
  (define σ
    (with-method ([f (cp f)])
      (f 'ℓ3 (f 'ℓ2 (f 'ℓ1 ⊤)))))

  (chk
   σ (hash 'x 1 'y 2 'z 5)
   ))
