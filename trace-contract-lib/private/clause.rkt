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
      (match-define (struct** subclause (collectors folder)) subcl)
      `[,(map object-name collectors) ,(object-name folder)]))
  `(accumulate ,init-acc ,@subclause-names))
