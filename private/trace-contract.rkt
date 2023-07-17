#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out trace-contract)
         make-trace-contract
         trace-contract-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         "checker.rkt"
         "clause.rkt"
         "collector-contract.rkt"
         "decl.rkt"
         "logger.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `trace-contract` is the main trace contract struct.
;;   - `decls` : [Listof Decl]
;;   - `global?` : Boolean
;;   - `make-inner` : Procedure
;;   - `clauses` : [Listof Clause]
;;   - `checker-hash` : Checker-Hash
;;   - `indy` : Any, contract-defining party
(struct trace-contract (decls global? make-inner clauses checker-hash indy))

(define (make-contract-property builder)
  (builder
   #:name (η trace-contract-name)
   #:late-neg-projection (η trace-contract-late-neg-projection)))

(struct chaperone-trace-contract trace-contract ()
  #:property prop:chaperone-contract
  (make-contract-property build-chaperone-contract-property))

(struct impersonator-trace-contract trace-contract ()
  #:property prop:contract
  (make-contract-property build-contract-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor

;; [Listof Decl] Boolean Procedure [Listof Clause] Any → Trace-Contract
(define (make-trace-contract decls global? inners clauses indy)
  (define checker-hash (and global? (make-checker-hash clauses)))
  (define ctcs
    (for/list ([inner (in-list inners)])
      (define make
        (if (chaperone-contract? (trace-contract-fake-inner decls inner))
            chaperone-trace-contract
            impersonator-trace-contract))
      (make decls global? inner clauses checker-hash indy)))
  (apply values ctcs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Trace-Contract → Sexpr
(define (trace-contract-name ctc)
  (match-define (struct** trace-contract (decls global? make-inner clauses)) ctc)
  (define inner-ctc (trace-contract-fake-inner decls make-inner))
  `(trace/c ,(map decl-contract-name decls)
     ,@(if global? '(#:global) null)
     ,(contract-name inner-ctc)
     ,@(map clause-contract-name clauses)))

;; Decls Procedure → Contract
;; Create a bunch of fake collectors to supply to the inner contract maker.
(define (trace-contract-fake-inner decls make-inner)
  (define fake-collectors (trace-contract-collectors (hash) decls #f))
  (apply make-inner fake-collectors))

;; Trace-Contract → Late-Neg-Proj
(define (trace-contract-late-neg-projection ctc)
  (match-define (struct** trace-contract (decls make-inner indy)) ctc)
  (λ (blm)
    (define blm* (blame-add-context blm "the inner contract of"))
    (λ (val neg)
      (log-trace-contract-info "projection")
      (define ch (trace-contract-checker-hash (trace-contract-init ctc)))
      (define collectors (trace-contract-collectors ch decls indy))
      (define inner-ctc (apply make-inner collectors))
      (define inner-late-neg (get/build-late-neg-projection inner-ctc))
      (define inner-proj (inner-late-neg blm*))
      (inner-proj val neg))))

;; Trace-Contract {Checker-Hash} → Trace-Contract
;; Set or create the checker hash if it doesn't exist already.
(define (trace-contract-init ctc [init-ch #f])
  (match-define (struct** trace-contract (clauses checker-hash)) ctc)
  (cond
    [checker-hash ctc]
    [else
     (define checker-hash* (or init-ch (make-checker-hash clauses)))
     (if (impersonator-trace-contract? ctc)
         (struct-copy impersonator-trace-contract ctc
                      [checker-hash #:parent trace-contract checker-hash*])
         (struct-copy chaperone-trace-contract ctc
                      [checker-hash #:parent trace-contract checker-hash*]))]))

;; Checker-Hash [Listof Decl] Any → [Listof Collector]
(define (trace-contract-collectors checker-hash decls indy)
  (for/list ([de (in-list decls)])
    (match-define (struct** decl (var ctc)) de)
    (make-collector var ctc (hash-ref checker-hash var #f) indy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (require (submod "decl.rkt" example)
           (submod "clause.rkt" example))

  (define pos/c
    (make-trace-contract (list x)
                         #f
                         (list (λ (x) (-> any/c x)))
                         (list positive-clause)
                         'indy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." example))

  (chk
   #:t (chaperone-contract? pos/c)
   #:t (impersonator-contract?
        (make-trace-contract '()
                             #f
                             (list (λ () (parametric->/c (A) (-> A A))))
                             '()
                             #f))

   (contract-name pos/c)
   '(trace/c ([x integer?])
      (-> any/c x)
      (accumulate 1 [(x) positive-folder]))

   #:do (define/contract (f x) pos/c x)
   (f 10)  10
   #:x (f -20)
   "accumulator: 11"
   ))
