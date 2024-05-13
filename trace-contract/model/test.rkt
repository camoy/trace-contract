#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/set
         racket/list
         racket/match
         redex/reduction-semantics
         redex-etc
         "syntax.rkt"
         "semantics.rkt"
         "compiler.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation functions

;; Expr → State
;; Inject a expression into a state.
(define (ς-inject e)
  (term (~ (curry ,e) ())))

;; Expr → State
;; Inject a expression into a state, after compilation.
(define (ς-inject-𝓒 e)
  (term (~ (𝓒 (curry ,e)) ())))

;; State → Expr
;; Project a expression out of a state.
(define (ς-project ς)
  (match-term
   evaluation ς
   [(~ e _) (term e)]))

;; Expr → Boolean
;; Returns if the expression is closed.
(define (closed? e)
  (set-empty? (term (fv ,e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation metafunctions

;; Expr → Expr
;; Returns the curried version of the given expression.
(define-metafunction surface
  curry : any -> any
  [(curry (any ... any_1 any_2 any_3))
   (left-assoc e ... e_1 e_2 e_3)
   (where (e ...) ((curry any) ...))
   (where e_1 (curry any_1))
   (where e_2 (curry any_2))
   (where e_3 (curry any_3))]
  [(curry (any ...)) ((curry any) ...)]
  [(curry any) any])

;; Expr → Expr
;; Left-associates the given application.
(define-metafunction surface
  left-assoc : any ... any any -> any
  [(left-assoc any ... any_1 any_2 any_3)
   ((left-assoc any ... any_1 any_2) any_3)]
  [(left-assoc any_1 any_2) (any_1 any_2)])

;; Expr → Set
;; Returns the set of free variables in the given expression.
(define-metafunction surface
  fv : any -> any
  [(fv x) ,(set (term x))]
  [(fv (λ x any)) ,(set-remove (term (fv any)) (term x))]
  [(fv (any ...)) ,(apply set-union (set) (term ((fv any) ...)))]
  [(fv any) ,(set)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simulation

;; Reduction-Relation → (Expr → Expr)
;; Returns if the compiler simulates the reduction sequence.
(define ((make-eval ↦) e #:answer? [answer? #t])
  (unless (closed? e)
    (raise-argument-error 'make-simulator "closed expression" e))
  (define ς (ς-inject e))
  (define ς° (ς-inject-𝓒 e))
  (unless (judgment-holds (ς≈ ,ς ,ς°))
    (error 'make-simulator
           "initial states aren't in simulation"))
  (let upper ([ς ς] [ς° ς°])
    (when (judgment-holds (𝓔≉ ,(ς-project ς)))
      (error 'make-simulator
             "compilation doesn't respect hole filling"))
    (match (apply-reduction-relation ↦ ς)
      [(list)
       (unless (empty? (apply-reduction-relation ↦ ς°))
         (error 'make-simulator
                "compilation is reducible when source isn't"))
       (define a (term (res->ans ,(ς-project ς))))
       (define a° (term (res->ans ,(ς-project ς°))))
       (and (equal? a a°)
            (if answer? a° (ς-project ς)))]
      [(list ς′)
       (let lower ([ς° ς°])
         (match-define (list ς°′)
           (apply-reduction-relation ↦ ς°))
         (if (judgment-holds (ς≈ ,ς′ ,ς°′))
             (upper ς′ ς°′)
             (lower ς°′)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define-metafunction surface
    Mon : any any -> any
    [(Mon any_κ any_e) (mon "ctc" "srv" "cli" any_κ any_e)])

  (define-metafunction surface
    *queue : any ... -> any
    [(*queue any ...)
     (*let ([t (queue)])
       (seq (add! t any) ...
            t))])

  (define-term id (λ x x))
  (define-term apply (λ f (f false)))
  (define-term const (λ x (λ y x)))
  (define-term is-null? (λ x (null? x)))
  (define-term not (λ x (if x false true)))
  (define-term Z
    (λ f ((λ x (f (λ v ((x x) v))))
          (λ x (f (λ v ((x x) v)))))))
  (define-term andmap
    (Z (λ* (recur f xs)
         (if (null? xs)
             true
             (if (f (head xs))
                 (recur f (tail xs))
                 false)))))
  (define-term alternate
    (Z (λ* (recur xs)
           (if (null? xs)
               true
               (*let ([xt (tail xs)])
                 (if (head xs)
                     (if (null? xt)
                         true
                         (if (head xt)
                             false
                             (recur (tail xt))))
                     false))))))

  (current-max-steps 200)
  (define ⇓ (make-eval ↦))

  (chk
   ;; basics
   (⇓ (term (id true)))
   (term true)
   (⇓ (term (id id)))
   (term "opaque")

   ;; list operations
   (⇓ (term (null? (queue))))
   (term true)
   (⇓ (term (*let ([t (queue)])
              (seq (add! t true)
                   (null? t)))))
   (term false)
   (⇓ (term (null? id)))
   (term false)
   (⇓ (term (head (*queue true))))
   (term true)
   (⇓ (term (null? (tail (*queue true)))))
   (term true)
   (⇓ (term (*let ([t (queue)])
              (seq (add! (id t) (id true))
                   (head t)))))
   (term true)
   (⇓ (term (head (tail (*queue true false)))))
   (term false)

   ;; boolean operations
   (⇓ (term (not true)))
   (term false)
   (⇓ (term (not false)))
   (term true)

   ;; recursion
   (⇓ (term (andmap id (*queue true true true))))
   (term true)
   (⇓ (term (andmap id (*queue true true false))))
   (term false)
   (⇓ (term (andmap not (*queue true true true))))
   (term false)
   (⇓ (term (andmap not (*queue true true false))))
   (term false)
   (⇓ (term (andmap not (*queue false false false))))
   (term true)
   (⇓ (term (alternate (*queue))))
   (term true)
   (⇓ (term (alternate (*queue true))))
   (term true)
   (⇓ (term (alternate (*queue false))))
   (term false)
   (⇓ (term (alternate (*queue true false))))
   (term true)
   (⇓ (term (alternate (*queue true true))))
   (term false)
   (⇓ (term (alternate (*queue true false true))))
   (term true)
   (⇓ (term (alternate (*queue true false false))))
   (term false)
   (⇓ (term (alternate (*queue true false true false))))
   (term true)

   ;; answers
   (⇓ (term (queue)))
   (term "opaque")
   (⇓ (term (flat (λ x true))))
   (term "opaque")
   (⇓ (term (->i true true)))
   (term "opaque")
   (⇓ (term (>>t (λ x true) (λ y true))))
   (term "opaque")

   ;; language errors
   (⇓ (term (add! true true)))
   (term (err "Λ" "†"))
   (⇓ (term (head true)))
   (term (err "Λ" "†"))
   (⇓ (term (tail false)))
   (term (err "Λ" "†"))
   (⇓ (term (true true)))
   (term (err "Λ" "†"))

   ;; late error
   (⇓ (term (->i (queue) (queue))))
   (term "opaque")
   (⇓ (term (>>t (queue) (queue))))
   (term "opaque")
   (⇓ (term ((Mon (-> (queue) (queue)) id) true)))
   (term (err "Λ" "ctc"))

   ;; flat contracts
   (⇓ (term (Mon true true)))
   (term true)
   (⇓ (term (Mon false true)))
   (term (err "ctc" "srv"))
   (⇓ (term (null? (Mon (flat (λ x (null? x))) (*queue)))))
   (term true)
   (⇓ (term (Mon (flat (λ x (null? x))) (*queue true))))
   (term (err "ctc" "srv"))

   ;; arrow contracts
   (⇓ (term (*let ([id/any (Mon (-> true true) id)])
              (id/any false))))
   (term false)
   (⇓ (term (*let ([id/neg (Mon (-> false true) id)])
              (id/neg false))))
   (term (err "ctc" "cli"))
   (⇓ (term (*let ([id/pos (Mon (-> true false) id)])
              (id/pos false))))
   (term (err "ctc" "srv"))
   (⇓ (term (*let ([id/any (Mon (-> (-> true true) (-> true true)) id)])
              ((id/any id) false))))
   (term false)
   (⇓ (term (*let ([apply/pos (Mon (-> (-> false true) true) apply)])
              (apply/pos id))))
   (term (err "ctc" "srv"))
   (⇓ (term (*let ([apply/neg (Mon (-> (-> true false) true) apply)])
              (apply/neg id))))
   (term (err "ctc" "cli"))
   (⇓ (term (*let ([apply/neg (Mon (-> true (-> false true)) (const id))])
              ((apply/neg false) false))))
   (term (err "ctc" "cli"))
   (⇓ (term (*let ([apply/pos (Mon (-> true (-> true false)) (const id))])
              ((apply/pos false) false))))
   (term (err "ctc" "srv"))

   ;; arrow contracts (first-order)
   (⇓ (term (Mon (-> true true) false)))
   (term (err "ctc" "srv"))
   (⇓ (term (*let ([id/neg (Mon (-> (-> true true) true) id)])
              (id/neg false))))
   (term (err "ctc" "cli"))

   ;; indy failure
   #:do (define-term with-null/c (->i true (λ f (null? (f (queue))))))
   #:do (define-term bad-null/c (->i (-> false true) (λ f (f (queue)))))
   (⇓ (term (*let ([id/ok (Mon with-null/c id)])
              ((id/ok id) false))))
   (term false)
   (⇓ (term (*let ([id/neg (Mon with-null/c id)])
              (id/neg (const false)))))
   (term (err "ctc" "srv"))
   (⇓ (term (*let ([id/indy (Mon bad-null/c id)])
              (id/indy id))))
   (term (err "ctc" "ctc"))

   ;; trace contract argument evaluation
   ;; (>>t e e)
   (⇓ (term ((Mon (>>t (id (λ x (-> x true)))
                       (id (λ y true)))
                  id)
             false)))
   (term false)

   ;; (>>t e v)
   (⇓ (term ((Mon (>>t (id (λ x (-> x true)))
                       (λ y true))
                  id)
             false)))
   (term false)

   ;; (>>t v e)
   (⇓ (term ((Mon (>>t (λ x (-> x true))
                       (id (λ y true)))
                  id)
             false)))
   (term false)

   ;; trace contract
   #:do
   (define-term id-arg/t
     (Mon (>>t (λ x (-> x true)) alternate) id))

   (⇓ (term (*let ([f id-arg/t])
              (seq (f true)
                   (f false)
                   (f true)
                   (f false)))))
   (term false)

   (⇓ (term (*let ([f id-arg/t])
              (seq (f true)
                   (f false)
                   (f true)
                   (f true)))))
   (term (err "ctc" "cli"))

   #:do
   (define-term id-ret/t
     (Mon (>>t (λ x (-> true x)) alternate) id))

   (⇓ (term (*let ([f id-ret/t])
              (seq (f true)
                   (f false)
                   (f true)
                   (f false)))))
   (term false)

   (⇓ (term (*let ([f id-ret/t])
              (seq (f true)
                   (f false)
                   (f true)
                   (f true)))))
   (term (err "ctc" "srv"))
   ))
