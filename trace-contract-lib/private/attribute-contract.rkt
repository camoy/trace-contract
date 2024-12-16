#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require meta
         racket/contract
         racket/class
         racket/linklet
         racket/match
         racket/syntax
         racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Predicates that are guaranteed to imply that a metadata may be attached.
(define KNOWN-IMPLIES-META
  (list object?))

;; Contract is one of the disjuncts in `meta?`.
(define contract-implies-meta?
  (flat-contract-with-explanation
   (λ (ctc)
     (or (contract-stronger? (coerce-contract/f ctc) meta?)
         (and (member ctc KNOWN-IMPLIES-META) #t)
         (λ (blm)
           (raise-blame-error blm ctc
                              '(expected: "a contract that implies meta?"
                                given: "~e")
                              ctc))))))

;; Even-length list of stash-key-and-value pairs.
(define attribute-value-list?
  (flat-named-contract
   'attribute-value-list?
   (λ (xs)
     (match xs
       ['() #t]
       [(list _) #f]
       [(list* x _ rst)
        (and (attribute? x)
             (attribute-value-list? rst))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; An opaque struct for attribute keys.
(struct attribute (name)
  #:property prop:object-name 0)

(define (make-attribute [name '???])
  (attribute name))

;; A `attribute-contract` contract associates mutable state with a wrapper and allows
;; contracts to modify and check that state.
;;   - `ctc` : Contract
;;   - `struct?` : Boolean
;;   - `key-init-hash` : [Hash Attribute-Contract-Key Any]
(struct attribute-contract (ctc struct? key-init-hash))

(define (make-contract-property builder)
  (builder
   #:name (λ (self) (attribute-contract-name self))
   #:late-neg-projection (λ (self) (attribute-contract-late-neg-projection self))))

(struct impersonator-attribute-contract attribute-contract ()
  #:property prop:contract
  (make-contract-property build-contract-property))

(struct chaperone-attribute-contract attribute-contract ()
  #:property prop:chaperone-contract
  (make-contract-property build-chaperone-contract-property))

(define-logger attribute-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attribute/c

;; Contract {Attribute-Contract-Key Any} ... → Attribute-Contract
(define (attribute/c ctc . key-init-pairs)
  (define maker
    (if (chaperone-contract? ctc)
        chaperone-attribute-contract
        impersonator-attribute-contract))
  (maker (coerce-contract/f ctc)
         (struct-predicate-procedure? ctc)
         (pairs->hash key-init-pairs)))

;; Attribute-Contract → SExpr
(define (attribute-contract-name self)
  (match-define (attribute-contract ctc _ ht) self)
  (define key-inits
    (for/list ([(k v) (in-hash ht)])
      (list (object-name k) v)))
  `(attribute/c ,(contract-name ctc) ,@(apply append key-inits)))

;; Attribute-Contract → Late-Neg-Projection
(define (attribute-contract-late-neg-projection self)
  (match-define (attribute-contract ctc st? ht) self)
  (define lnp (get/build-late-neg-projection ctc))
  (λ (blm)
    (define lnp+blm (lnp blm))
    (λ (val neg)
      (log-attribute-contract-info "projection")
      (define st (and st? (unsafe-struct-type val)))
      (for/fold ([val (lnp+blm val neg)])
                ([(key init-acc) (in-hash ht)])
        (define (update accs) (cons (box init-acc) accs))
        (if st
            (meta-update val key update null #:struct-type st)
            (meta-update val key update null))))))

;; [Listof Any] → [Hash Any Any]
;; Given an even-length list, constructs a hash.
(define (pairs->hash ps)
  (let go ([ps ps])
    (match ps
      ['() (hash)]
      [(list* x y rst) (hash-set (go rst) x y)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shenanigans

;; For `record?` values, this is `record-rtd`.
(define (unsafe-struct-type val)
  (unsafe-struct-ref (strip-impersonator val) -1))

;; From `rumble/impersonator.ss`.
(define (strip-impersonator v)
  (if (impersonator? v)
      (impersonator-val v)
      v))

;; Trick to get `impersonator-val` from `'#%internal`.
(define impersonator-val
  (instance-variable-value
   (instantiate-linklet
    (compile-linklet
     '(linklet () (x) (define-values (x) impersonator-val)))
    '())
   'x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other attribute functions

;; Attribute-Contract-Key → Contract
(define (attribute-present/c key)
  (flat-named-contract
   `(attribute-present/c ,(object-name key))
   (λ (val) (attribute-present? key val))))

;; Attribute-Contract-Key Value → Boolean
(define (attribute-present? key val)
  (meta-has-key? val key))

;; Attribute-Contract-Key Any → Contract
(define (attribute-set/c key new-val)
  (make-chaperone-contract
   #:name `(attribute-set/c ,(object-name key) ,(object-name new-val))
   #:first-order (λ (val) (attribute-present? key val))
   #:late-neg-projection
   (λ (blm)
     (λ (val neg)
       (check-missing-meta key blm val neg)
       (unless (blame-replaced-negative? blm)
         (attribute-set! key val new-val))
       val))))

;; Attribute-Contract-Key Any Any → Void
(define (attribute-set! key val new-val)
  (define acc-boxes (meta-ref val key))
  (for ([acc-box (in-list acc-boxes)])
    (set-box! acc-box new-val)))

;; Attribute-Contract-Key Procedure → Contract
(define (attribute-update/c key proc)
  (make-chaperone-contract
   #:name `(attribute-update/c ,(object-name key ) ,(object-name proc))
   #:first-order (λ (val) (attribute-present? key val))
   #:late-neg-projection
   (λ (blm)
     (λ (val neg)
       (check-missing-meta key blm val neg)
       (unless (blame-replaced-negative? blm)
         (attribute-update! key val proc))
       val))))

;; Attribute-Contract-Key Any Procedure → Void
(define (attribute-update! key val proc)
  (define acc-boxes (meta-ref val key))
  (log-attribute-contract-info "check")
  (for ([acc-box (in-list acc-boxes)])
    (set-box! acc-box (proc (unbox acc-box)))))

;; Attribute-Contract-Key Procedure → Contract
(define (attribute-satisfies/c key proc)
  (make-flat-contract
   #:name `(attribute-satisfies/c ,(object-name key) ,(object-name proc))
   #:first-order (λ (val) (attribute-satisfies? key val proc))
   #:late-neg-projection
   (λ (blm)
     (λ (val neg)
       (check-missing-meta key blm val neg)
       (unless (blame-replaced-negative? blm)
         (log-attribute-contract-info "check")
         (define acc-boxes (meta-ref val key))
         (for ([acc-box (in-list acc-boxes)])
           (define acc (unbox acc-box))
           (unless (proc acc)
             (raise-blame-error
              #:missing-party neg
              blm val
              "accumulator ~a does not satisfy ~a"
              acc (object-name proc)))))
       val))))

;; Attribute-Contract-Key Any (Any → Boolean) → Boolean
(define (attribute-satisfies? key val pred?)
  (define acc-boxes (meta-ref val key #f))
  (and acc-boxes
       (for/and ([acc-box (in-list acc-boxes)])
         (pred? (unbox acc-box)))))

;; Attribute-Contract-Key Blame Any Party → Void
(define (check-missing-meta key blm val neg)
  (unless (attribute-present? key val)
    (raise-blame-error
     #:missing-party neg
     blm val
     "~a does not have key ~a"
     val (object-name key))))
