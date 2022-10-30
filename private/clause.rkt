#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out clause)
         make-clause
         clause-contract-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         "subclause.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `clause` represents the syntax of an `accumulate` clause.
;;   - `init-acc` : Any
;;   - `subclauses` : [Listof Subclause]
(struct clause (init-acc subclauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Any Subclause ... → Clause
(define (make-clause init-acc . subclauses)
  (clause init-acc subclauses))

;; Clause → Sexpr
;; Used for `contract-name`.
(define (clause-contract-name cl)
  (match-define (struct** clause (init-acc subclauses)) cl)
  (define subclause-names
    (for/list ([subcl (in-list subclauses)])
      (match-define (struct** subclause (vars folder)) subcl)
      `[,vars ,(object-name folder)]))
  `(accumulate ,init-acc ,@subclause-names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (require "fail.rkt")

  (define (positive-folder acc x)
    (define acc* (+ acc x))
    (if (positive? acc*) acc* (make-fail)))
  (define positive-clause
    (make-clause 1 (subclause '(x) positive-folder)))

  (define (even-folder acc x y)
    (define acc* (+ acc x y))
    (if (even? acc*) acc* (make-fail)))
  (define even-clause
    (make-clause 0 (subclause '(x y) even-folder)))
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
