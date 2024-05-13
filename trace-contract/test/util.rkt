#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract/private/util
         racket/match
         racket/set)

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
