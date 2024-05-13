#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out collector-contract)
         (struct-out impersonator-collector-contract)
         (struct-out chaperone-collector-contract)
         make-collector
         clause-register!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         racket/set
         "checker.rkt"
         "clause.rkt"
         "subclause.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `collector-contract` is used to communicate values to checkers.
;;   - `name` : SExpr
;;   - `pre` : Contract
;;   - `mapper` : Procedure
;;   - `checkers` : [Setof Checkers], checkers dependent this on collector
;;   - `indy` : Any
(struct collector-contract (name pre mapper checkers indy)
  #:property prop:object-name 0)

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

;; SExpr Contract Any → Collector
(define (make-collector name pre indy)
  (define pre* (coerce-contract/f pre))
  (define make
    (if (chaperone-contract? pre*)
        chaperone-collector-contract
        impersonator-collector-contract))
  (make name pre* #f (mutable-seteq) indy))

;; Collector → Late-Neg-Projection
(define (collector-contract-late-neg-projection ctc)
  (match-define (struct** collector-contract (pre mapper checkers indy)) ctc)
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
        (update! ch ctc blm val-mapped neg))
      (proj val neg))))

;; Clause → Void
(define (clause-register! cl)
  (match-define (struct** clause (init-acc subclauses)) cl)
  (define acc-box (box init-acc))
  (define acc-sem (make-semaphore 1))
  (for/fold ([ht (hash)])
            ([subcl (in-list subclauses)])
    (match-define (struct** subclause (collectors folder)) subcl)
    (define blame-box (box #f))
    (define val-hash (and (> (length collectors) 1) (make-hash)))
    (define folder* (checker-wrap-folder folder))
    (define ch (checker acc-box acc-sem init-acc collectors blame-box val-hash folder*))
    (for ([collector (in-list collectors)])
      (set-add! (collector-contract-checkers collector) ch))))
