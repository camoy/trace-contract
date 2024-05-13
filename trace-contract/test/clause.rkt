#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract/private/clause
         trace-contract/private/fail
         trace-contract/private/subclause)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (define (positive-folder acc x)
    (define acc* (+ acc x))
    (if (positive? acc*) acc* (make-fail)))
  (define (positive-clause x)
    (make-clause 1 (subclause (list x) positive-folder)))

  (define (even-folder acc x y)
    (define acc* (+ acc x y))
    (if (even? acc*) acc* (make-fail)))
  (define (even-clause x y)
    (make-clause 0 (subclause (list x y) even-folder)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." example))

  (chk
   (clause-contract-name positive-clause)
   '(accumulate 1 [(x) positive-folder])

   (clause-contract-name even-clause)
   '(accumulate 0 [(x y) even-folder])
   ))
