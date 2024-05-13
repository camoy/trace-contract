#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide surface
         evaluation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; surface

(define-language surface
  [e ::=
     b
     x
     f
     (o e)
     (if e e e)
     (e e)
     (queue)
     (add! e e)
     (flat e)
     (->i e e)
     (>>t e e)
     (mon j k l e e)]
  [b ::= true false]
  [f ::= (λ x e)]
  [o ::= null? head tail]

  [x y z ::= variable-not-otherwise-mentioned]
  [j k l ::= string]

  #:binding-forms
  (λ x e #:refers-to x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation

(define-extended-language evaluation surface
  [ς ::= (~ e Σ)]
  [e ::=
     ....
     α
     l
     (>>c v v)
     (mon j k e e)
     (err j k)]
  [f ::=
     ....
     (grd j k v v)]

  [r ::= v (err j k)]
  [v ::= b f κ α l]
  [κ ::=
     b
     (flat v)
     (->i v v)
     (>>t v v)
     (>>c v v)]

  [a ::=
     b
     "opaque"
     (err j k)]

  [E ::=
     hole
     (o E)
     (if E e e)
     (E e)
     (v E)
     (add! E e)
     (add! v E)
     (flat E)
     (->i E e)
     (->i v E)
     (>>t E e)
     (>>t v E)
     (mon j k E e)
     (mon j k v E)]

  [Σ ::= ([α u] ...)]
  [α ::= natural]
  [u ::= null (cons v v)]
  [w ::= v u])
