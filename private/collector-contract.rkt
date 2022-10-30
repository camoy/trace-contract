#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out collector-contract)
         (struct-out impersonator-collector-contract)
         (struct-out chaperone-collector-contract)
         make-collector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         racket/set
         "checker.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `collector-contract` is used to communicate values to checkers.
;;   - `var` : Symbol
;;   - `pre` : Contract
;;   - `mapper` : Procedure
;;   - `checkers` : [Setof Checkers], checkers dependent this on collector
;;   - `indy` : Any
(struct collector-contract (var pre mapper checkers indy))

(define (make-contract-property builder)
  (builder
   #:name (η collector-contract-name)
   #:late-neg-projection (η collector-contract-late-neg-projection)))

(struct impersonator-collector-contract collector-contract ()
  #:property prop:contract
  (make-contract-property build-contract-property))

(struct chaperone-collector-contract collector-contract ()
  #:property prop:chaperone-contract
  (make-contract-property build-chaperone-contract-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Symbol Contract [Setof Checkers] Any → Collector
(define (make-collector var pre checkers indy)
  (define pre* (coerce-contract/f pre))
  (define make
    (if (chaperone-contract? pre*)
        chaperone-collector-contract
        impersonator-collector-contract))
  (make var pre* #f checkers indy))

;; Collector → SExpr
(define (collector-contract-name ctc)
  (match-define (struct** collector-contract (var mapper)) ctc)
  (if mapper
      `(map/t ,(object-name mapper) ,var)
      var))

;; Collector → Late-Neg-Projection
(define (collector-contract-late-neg-projection ctc)
  (match-define (collector-contract var pre mapper checkers indy) ctc)
  (define late-neg-proj (get/build-late-neg-projection pre))
  (λ (blm)
    (define indy-blm (blame-replace-negative blm indy))
    (define indy-proj (late-neg-proj indy-blm))
    (define proj (late-neg-proj blm))
    (define update!
      (if (blame-replaced-negative? blm) void checker-update!))
    (λ (val neg)
      (define val-indy (indy-proj val neg))
      (define val-mapped (if mapper (mapper val-indy) val-indy))
      (for ([ch (in-set checkers)])
        (update! ch var blm val-mapped neg))
      (proj val neg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example

(module+ examples
  (provide (all-defined-out))
  (define empty-collector (make-collector 'x any/c (set) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           (submod ".." examples))

  (chk
   (collector-contract-name empty-collector)  'x
   ))
