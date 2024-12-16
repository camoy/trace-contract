#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide δ
         ↦)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require redex-etc
         redex/reduction-semantics
         "while.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation syntax

(define-extended-language While+Σ While
  [ς ::= (S Σ)]
  [Σ ::= ([X integer] ...)])

(define-extended-language While+Σ+E While+Σ
  [E ::=
     hole
     (AO₂ E A) (AO₂ integer E)
     (BO₁ E)
     (BO₂ E B) (BO₂ boolean E)
     (RO₂ E A) (RO₂ integer E)
     {(:= X E) ℓ}
     (seq E S ...)
     (if {E ℓ} S S)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunctions

(define-metafunction While
  δ : M -> M
  [(δ (+ integer_0 integer_1)) ,(+ (term integer_0) (term integer_1))]
  [(δ (- integer_0 integer_1)) ,(- (term integer_0) (term integer_1))]
  [(δ (* integer_0 integer_1)) ,(* (term integer_0) (term integer_1))]
  [(δ (¬ boolean_0))           ,(not (term boolean_0))]
  [(δ (∧ boolean_0 boolean_1)) ,(and (term boolean_0) (term boolean_1))]
  [(δ (∨ boolean_0 boolean_1)) ,(or (term boolean_0) (term boolean_1))]
  [(δ (< integer_0 integer_1)) ,(< (term integer_0) (term integer_1))]
  [(δ (= integer_0 integer_1)) ,(= (term integer_0) (term integer_1))]
  [(δ (> integer_0 integer_1)) ,(> (term integer_0) (term integer_1))])

(define-metafunction While+Σ
  inj : S -> ς
  [(inj S) (S ())])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduction relation

(define ↦
  (reduction-relation
   While+Σ+E
   #:domain ς

   [m (O V_0 ...) (δ (O V_0 ...))]
   [s (if {#t _} S_0 S_1) S_0]
   [s (if {#f _} S_0 S_1) S_1]
   [s (while {B _} S)
      (if {B ℓ0} (seq S (while {B ℓ0} S)) {skip ℓ0})]
   [s (seq {skip _} S_0 S_1 ...)
      (seq S_0 S_1 ...)]
   [s (seq {skip _}) {skip ℓ0}]

   [--> ((in-hole E X) Σ) ((in-hole E (lookup Σ X)) Σ)]
   [--> ((in-hole E {(:= X number) ℓ}) Σ)
        ((in-hole E {skip ℓ}) (ext Σ [X number]))]

   with
   [(--> ((in-hole E x) Σ) ((in-hole E y) Σ))
    (m x y)]
   [(--> ((in-hole E x) Σ) ((in-hole E y) Σ))
    (s x y)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#;(module+ test
  (require (for-syntax racket/base syntax/parse)
           racket/list)

  (define-syntax (chk-eval stx)
    (syntax-parse stx
      [(_ (~seq stmt σ) ...)
       #'(begin
           (test-->> ↦ #:equiv store-equal?
                     (term (inj stmt))
                     (term ({skip ℓ1} σ))) ...)]))

  (define (store-equal? x y)
    (equal? (second x) (second y)))

  (chk-eval
   {(:= x 3) ℓ1}  ([x 3])
   {skip ℓ1}  ()

   (seq
    {(:= x 1) ℓ1}
    {(:= y 2) ℓ2}
    {(:= x 42) ℓ3})
   ([y 2] [x 42])

   (if {(= 2 (+ 1 1)) ℓ1}
       {(:= x 42) ℓ2}
       {(:= x -1) ℓ3})
   ([x 42])

   (seq
    {(:= x 10) ℓ1}
    (if {(> x 10) ℓ2}
        {(:= x 42) ℓ3}
        {(:= x -1) ℓ4}))
   ([x -1])

   (seq
    {(:= x 5) ℓ1}
    {(:= y 1) ℓ2}
    {(:= z 1) ℓ3}
    (while {(< z x) ℓ4}
      (seq
       {(:= z (+ z 1)) ℓ5}
       {(:= y (* y z)) ℓ6})))
   ([z 5] [y 120] [x 5]))
  )
