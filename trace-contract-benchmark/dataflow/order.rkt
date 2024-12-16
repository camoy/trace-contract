#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide partial-order<%>
         join-semilattice<%>
         meet-semilattice<%>
         lattice<%>
         complete-lattice<%>
         complete-lattice-mixin
         ∂-lattice-mixin

         powerset%
         discrete%
         bounded-discrete%
         bound-above
         bound-below
         pointwise-function-space%
         numbers%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interfaces

;; A reflexive, anti-symmetric, transitive relation.
(define partial-order<%>
  (interface ()
    ⊑))

;; A partial order equipped with a join.
(define join-semilattice<%>
  (interface (partial-order<%>)
    ⊔))

;; A partial order equipped with a meet.
(define meet-semilattice<%>
  (interface (partial-order<%>)
    ⊓))

;; A partially ordered set such that every pair of elements has a join and meet.
(define lattice<%>
  (interface (join-semilattice<%> meet-semilattice<%>)))

(define topped<%>
  (interface (partial-order<%>)
    ⊤))

(define bottomed<%>
  (interface (partial-order<%>)
    ⊥))

;; A partially ordered set such that every subset has join and meet.
(define complete-lattice<%>
  (interface (lattice<%> topped<%> bottomed<%>)))

;; Calculates extremal lattice elements by joining and meeting the empty set.
(define complete-lattice-mixin
  (mixin (lattice<%>) (complete-lattice<%>)
    (inherit ⊔ ⊓)
    (super-new)
    (define/public (⊤) (⊓))
    (define/public (⊥) (⊔))))

;; Calculates dual lattice.
(define ∂-lattice-mixin
  (mixin (lattice<%>) (lattice<%>)
    (super-new)
    (define/override (⊑ x y) (super ⊑ y x))
    (define/override (⊔ . rest) (super ⊓ . rest))
    (define/override (⊓ . rest) (super ⊔ . rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

;; Lattice%
;; The complete powerset lattice over the given universe.
(define powerset%
  (complete-lattice-mixin
   (class* object% (lattice<%>)
     (init universe)
     (define current-universe universe)
     (super-new)
     (define/public (⊑ x y)
       (subset? x y))
     (define/public ⊔
       (case-lambda
         [() (set)]
         [xs (apply set-union xs)]))
     (define/public ⊓
       (case-lambda
         [() current-universe]
         [xs (apply set-intersect xs)])))))

;; Lattice% → Lattice%
;; Given a partial order P, returns the linear sum: P ⊕ {⊤}.
(define bound-above
  (mixin (partial-order<%>) (partial-order<%> topped<%>)
    (super-new)
    (define -⊤ (gensym '⊤))
    (define/public (⊤) -⊤)
    (define/override (⊑ x y)
      (or (eq? y -⊤)
          (super ⊑ x y)))))

;; Lattice% → Lattice%
;; Given a partial order P, returns the linear sum: {⊥} ⊕ P. Also known as
;; "P lifted."
(define bound-below
  (mixin (partial-order<%>) (partial-order<%> bottomed<%>)
    (super-new)
    (define -⊥ (gensym '⊥))
    (define/public (⊥) -⊥)
    (define/override (⊑ x y)
      (or (eq? x -⊥)
          (super ⊑ x y)))))

;; Partial-Order%
;; The discrete order.
(define discrete%
  (class* object% (partial-order<%>)
    (super-new)
    (define/public (⊑ x y)
      (equal? x y))))

;; Complete-Lattice%
;; The bounded discrete lattice.
(define bounded-discrete%
  (class* (bound-above (bound-below discrete%)) (complete-lattice<%>)
    (inherit ⊤ ⊥)
    (super-new)
    (define/public ⊔
      (case-lambda
        [() (⊥)]
        [(x . xt)
         (define xt* (send/apply this ⊔ xt))
         (cond
           [(equal? xt* (⊥)) x]
           [(equal? x (⊥)) xt*]
           [(equal? x xt*) x]
           [else (⊤)])]))
    (define/public ⊓
      (case-lambda
        [() (⊤)]
        [(x . xt)
         (define xt* (send/apply this ⊓ xt))
         (cond
           [(equal? xt* (⊤)) x]
           [(equal? x (⊤)) xt*]
           [(equal? x xt*) x]
           [else (⊥)])]))))

;; Set Complete-Lattice% → Complete-Lattice%
;; Constructs the pointwise ordered function space from a finite domain.
(define pointwise-function-space%
  (complete-lattice-mixin
   (class* object% (lattice<%>)
     (init domain range)
     (super-new)

     (define current-domain domain)
     (define current-range  range)

     (define/public (⊑ f g)
       (with-method ([⊑ (current-range ⊑)])
         (for/and ([x (in-set current-domain)])
           (⊑ (hash-ref f x) (hash-ref g x)))))

     (define/public ⊔
       (case-lambda
         [()
          (for/hash ([x (in-set current-domain)])
            (values x (send current-range ⊥)))]
         [(f . ft)
          (define g (send/apply current-range ⊔ ft))
          (for/hash ([x (in-set current-domain)])
            (values x (send current-range ⊔ (hash-ref f x) (hash-ref g x))))]))

     (define/public ⊓
       (case-lambda
         [()
          (for/hash ([x (in-set current-domain)])
            (values x (send current-range ⊤)))]
         [(f . ft)
          (define g (send/apply current-range ⊓ ft))
          (for/hash ([x (in-set current-domain)])
            (values x (send current-range ⊓ (hash-ref f x) (hash-ref g x))))]))
     )))

;; Partial-Order%
;; The canonical ordering of numbers.
(define numbers%
  (class* object% (partial-order<%>)
    (super-new)
    (define/public (⊑ x y)
      (<= x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#;(module+ test
  (require chk)

  (define bounded-dis (new bounded-discrete%))
  (define-values (bd-⊤ bd-⊥)
    (values (send bounded-dis ⊤)
            (send bounded-dis ⊥)))

  (chk
   (send bounded-dis ⊔ bd-⊥ 1) 1
   (send bounded-dis ⊔ 1 1) 1
   (send bounded-dis ⊔ 1 2) bd-⊤
   (send bounded-dis ⊔ 1 1 bd-⊥) 1
   (send bounded-dis ⊓ 1 1) 1
   (send bounded-dis ⊓ 1 2) bd-⊥
   (send bounded-dis ⊓ 1 1 bd-⊥) bd-⊥
   ))
