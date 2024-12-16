#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide While
         variables
         label->block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/set
         redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(define-language While
  [A   ::= X integer (AO₂ A A)]
  [AO₂ ::= + - *]

  [B   ::= boolean (BO₁ B) (BO₂ B B) (RO₂ A A)]
  [BO₁ ::= ¬]
  [BO₂ ::= ∧ ∨]
  [RO₂ ::= < = >]

  [V ::= integer boolean]
  [M ::= A B]
  [O ::= AO₂ BO₁ BO₂ RO₂]
  [X ::= variable-not-otherwise-mentioned]
  [ℓ ::= (variable-prefix ℓ)]
  [S ::=
     {(:= X A) ℓ}
     {skip ℓ}
     (seq S S ...)
     (if {B ℓ} S S)
     (while {B ℓ} S)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables

;; Block → [Setof Var]
;; Given a block, returns all variables.
(define (variables block)
  (list->set (term (vars ,block))))

;; Block → [Listof Var]
;; Given a block, returns all variables contained therein.
(define-metafunction While
  vars : any -> (X ...)
  [(vars ℓ) ()]
  [(vars X) (X)]
  [(vars (any ...))
   (X ... ...)
   (where ((X ...) ...) ((vars any) ...))]
  [(vars _) ()])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; label->block

;;  Label [Setof Block] → Block
;; Given a set of blocks returns the block with label ℓ.
(define (label->block ℓ blocks)
  (for/first ([block (in-set blocks)]
              #:when (equal? ℓ (term (block->label ,block))))
    block))

;; Block → Label
;; Given a block, returns its label.
(define-metafunction While
  block->label : any -> ℓ
  [(block->label {_ ℓ}) ℓ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#;(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           chk)

  (define-syntax (chk-while stx)
    (syntax-parse stx
      [(_ (~seq nonterm m) ...)
       #'(begin
           (chk #:t (redex-match? While nonterm (term m))) ...)]))

  (define-syntax (chk-not-while stx)
    (syntax-parse stx
      [(_ (~seq nonterm m) ...)
       #'(begin
           (chk #:! #:t (redex-match? While nonterm (term m))) ...)]))

  (chk-while
   A x
   A 42
   A (+ 1 42)
   A (- (+ 42 3) (* 7 12))
   A (* -1 7)
   B #t
   B #f
   B (¬ #t)
   B (∨ (∧ #t #f) #f)
   B (= 3 (+ 1 2))
   S (seq
       {(:= x 0) ℓ1}
       (while {(< x 10) ℓ2}
         {(:= x (+ x 1)) ℓ3}))
   S (seq
       {(:= x 42) ℓ1}
       (if {(> x 10) ℓ2}
           {(:= x 10) ℓ3}
           {skip ℓ4}))
   )

  (chk-not-while
   A #t
   A 43/2
   A (+ 1 42 7)
   A (- (+ 42 3) (/ 7 12))
   B x
   B (¬ ¬)
   B (= #t (+ 1 2))
   S (seq)
   S {(:= 10 42) ℓ1}
   S (if {#t ℓ1} {(:= x 10) ℓ2})
   S (if {#t ℓ1} {1 ℓ2} {2 ℓ3})
   )

  (chk
   (variables (term (+ x y)))
   (set 'x 'y)

   (variables (term (∨ (= x y) (> (+ y z) 1))))
   (set 'x 'y 'z)

   (variables (term (∨ (= x (* y z)) (= w w))))
   (set 'x 'y 'z 'w)

   (variables (term {skip ℓ1}))
   (set)

   (variables
    (term (seq {(:= a 10) ℓ1}
               (if {(= b c) ℓ2}
                   {(:= d (+ e f)) ℓ3}
                   {skip ℓ4}))))
   (set 'a 'b 'c 'd 'e 'f)

   #:do
   (define test-blocks
     (list->set '({(:= x 1) ℓ1} {(:= y 2) ℓ2})))

   (label->block 'ℓ1 test-blocks) '{(:= x 1) ℓ1}
   (label->block 'ℓ2 test-blocks) '{(:= y 2) ℓ2}
   ))
