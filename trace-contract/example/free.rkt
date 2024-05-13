#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require automata/machine
         automata/re
         automata/re-ext
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; A regular expression for recognizing that a call to next must be preceded by
;; a call to has-next.
(define ALTERNATING-RE
  (re (seq (star (seq 'a 'f))
           (opt 'a))))

(define (alternating-re-next acc t)
  (match-define (list i val) t)
  (define st (hash-ref acc i (λ () ALTERNATING-RE)))
  (define st* (st val))
  (if (machine-accepting? st*)
      (hash-set acc i st*)
      (fail)))

;; For `alloc` and `free` respectively. Usual memory rules apply.
(define (make-contracts)
  (trace/c ([x integer?])
    #:global
    (values (-> integer? (list/t x 'a))
            (-> (list/t x 'f) any))
    (accumulate (hash) [(x) alternating-re-next])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/strip-context)
           chk)

  (define-syntax (define-alloc-free stx)
    (replace-context
     stx
     #'(define-values/contract (alloc free)
         (make-contracts)
         (values values void))))

  (chk
   #:t (machine-accepts? ALTERNATING-RE '())
   #:t (machine-accepts? ALTERNATING-RE '(a))
   #:t (machine-accepts? ALTERNATING-RE '(a f a f a f))
   #:t (machine-accepts? ALTERNATING-RE '(a f a f a f a))
   #:! #:t (machine-accepts? ALTERNATING-RE '(a f a f a f f))

   #:t
   (begin
     (define-alloc-free)
     (alloc 1)
     (alloc 2)
     (free 1)
     (free 2)
     (alloc 1)
     (free 1))
   #:x
   (begin
     (define-alloc-free)
     (free 1))
   (trace-exn? (current-contract-region)
               '(definition free)
               "given: '(1 f)")
   #:x
   (begin
     (define-alloc-free)
     (alloc 1)
     (free 1)
     (free 1))
   (trace-exn? (current-contract-region)
               '(definition free)
               "given: '(1 f)")

   ;; This is simulating a faulty `alloc` that returns the same address before
   ;; being freed. That's why it blames `alloc` (and is correct).
   #:x
   (begin
     (define-alloc-free)
     (alloc 1)
     (alloc 1))
   (trace-exn? '(definition alloc)
               (current-contract-region)
               "produced: '(1 a)")
   ))
