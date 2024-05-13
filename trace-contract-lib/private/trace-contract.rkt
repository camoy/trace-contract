#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out trace-contract)
         make-trace-contract)

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
;;   - `name` : SExpr
;;   - `decls` : [Listof Decl]
;;   - `make-inner` : Procedure
;;   - `indy` : Any, contract-defining party
(struct trace-contract (name decls make-inner indy))

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

;; SExpr [Listof Decl] Boolean Procedure Any → Trace-Contract ...
(define (make-trace-contract name decls global? inners indy)
  (define collectors (trace-contract-collectors decls indy))
  (for/values ([inner (in-list inners)])
    (define body-ctc (apply inner collectors))
    (define make
      (if (chaperone-contract? body-ctc)
          chaperone-trace-contract
          impersonator-trace-contract))
    (if global?
        (make name decls (λ _ body-ctc) indy)
        (make name decls inner indy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Trace-Contract → Late-Neg-Proj
(define (trace-contract-late-neg-projection ctc)
  (match-define (struct** trace-contract (decls make-inner indy)) ctc)
  (λ (blm)
    (define blm* (blame-add-context blm "the inner contract of"))
    (λ (val neg)
      (when logger-enable?
        (log-trace-contract-info "projection"))
      (define collectors (trace-contract-collectors decls indy))
      (define inner-ctc (apply make-inner collectors))
      (define inner-late-neg (get/build-late-neg-projection inner-ctc))
      (define inner-proj (inner-late-neg blm*))
      (inner-proj val neg))))

;; [Listof Decl] Any → [Listof Collector]
(define (trace-contract-collectors decls indy)
  (for/list ([de (in-list decls)])
    (match-define (struct** decl (var ctc)) de)
    (make-collector var ctc indy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (require (submod "decl.rkt" example)
           (submod "clause.rkt" example))

  (define pos/c
    (make-trace-contract
     '(trace/c ((x integer?)) (-> any/c x) (accumulate 1 ((x) positive-folder)))
     (list x)
     #f
     (list (λ (x)
             (clause-register! (positive-clause x))
             (-> any/c x)))
     'indy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." example))

  (chk
   #:t (chaperone-contract? pos/c)
   #:t (impersonator-contract?
        (make-trace-contract 'trace/c
                             '()
                             #f
                             (list (λ () (parametric->/c (A) (-> A A))))
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
