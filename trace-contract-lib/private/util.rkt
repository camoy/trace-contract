#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide η
         struct**
         hash-add-set
         for/values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; η-expansion (used for struct properties).
(define-syntax (η stx)
  (syntax-parse stx
    [(_ f:id) #'(λ (x) (f x))]))

;; Match expander shorthand for structs where we just want to bind fields.
(define-match-expander struct**
  (syntax-parser
    [(_ st:id (fld:id ...))
     #'(struct* st ([fld fld] ...))]))

;; [Hash K [Setof V]] [Listof K] V → [Hash K [Setof V]]
;; Adds the given value to each of the sets associated with the keys.
(define (hash-add-set ht keys val)
  (for/fold ([ht ht])
            ([key (in-list keys)])
    (hash-update ht key (λ (s) (set-add s val)) (set val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(define-syntax-rule (for/values seqs body ...)
  (apply values (for/list seqs body ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (struct foo (bar baz))
   #:do (match-define (struct** foo (baz)) (foo 1 2))
   baz 2

   #:do (define f (η -))
   (f 5) -5

   #:do (define ht (hash 'a (set 1 2) 'b (set 3) 'c (set)))
   (hash-add-set ht '(a c) 4)
   (hash 'a (set 1 2 4) 'b (set 3) 'c (set 4))

   (for/values ([x '(1 2 3)]) x)
   (values 1 2 3)
   ))
