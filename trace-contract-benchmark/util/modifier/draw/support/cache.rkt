#lang racket/base

(provide cache/c
         cache/c*)

(require racket/contract)

(define (cache/c -ctc)
  (define ctc (coerce-contract/f -ctc))
  (define lnp (get/build-late-neg-projection ctc))
  (define cache (make-weak-hasheq))
  (make-contract
   #:name `(cache/c ,(contract-name ctc))
   #:late-neg-projection
   (λ (blm)
     (define lnp+blm (lnp blm))
     (λ (val neg)
       (define (to-set) (lnp+blm val neg))
       (hash-ref! cache val to-set)))))

(define (cache/c*)
  (define cache (make-weak-hasheq))
  (λ (make-ctc)
    (make-contract
     #:late-neg-projection
     (λ (blm)
       (λ (val neg)
         (define (to-set)
           (define ctc (coerce-contract/f (make-ctc)))
           (define lnp (get/build-late-neg-projection ctc))
           (define lnp+blm (lnp blm))
           (lnp+blm val neg))
         (hash-ref! cache val to-set))))))
