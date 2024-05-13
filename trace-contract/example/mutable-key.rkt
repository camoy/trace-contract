#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (sequence-ok? t)
  (for/fold ([s (set)])
            ([x t])
    #:break (not s)
    (match x
      [(cons 'set e) (set-add s e)]
      [(cons 'mut e) (and (not (set-member? s e)) s)])))

;; For hash operations to make sure that no keys are never mutated.
(define-values (hash-set/c mutable/c)
  (trace/c ([t any/c])
    #:global
    (values (-> hash? (list/t 'set t) any/c any)
            (list/t 'mut t))
    (full (t) sequence-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define/contract hash-set!* hash-set/c hash-set!)
  (define/contract string-set!*
    (-> (and/c string? mutable/c)
        exact-nonnegative-integer?
        char?
        any)
    string-set!)

  (define foo (string #\f #\o #\o))
  (define bar (string #\b #\a #\r))
  (define h (make-hash))

  (hash-set!* h bar 3)
  (string-set!* foo 1 #\z)
  (chk
   (hash-ref h bar) 3
   foo "fzo"
   #:x
   (string-set!* bar 1 #\u)
   (trace-exn? (current-contract-region)
               '(definition string-set!*)
               "t: ((set bar) (mut fzo) (mut bar))")
   ))

;; Example of bad behavior:
#|
(define foo (string #\f #\o #\o))
(define bar (string #\b #\a #\r))
(define h (make-hash))

(hash-set! h foo 1)
(hash-set! h bar 2)

(string-set! bar 1 #\u)
(for ([k (in-hash-keys h)])
  (displayln k))
(hash-ref h "bur")
|#
