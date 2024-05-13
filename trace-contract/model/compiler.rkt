#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide 𝓒
         𝓔
         ς≈
         𝓔≉)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/list
         racket/match
         redex/reduction-semantics
         redex-etc
         "syntax.rkt"
         "semantics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(define-metafunction evaluation
  𝓒 : e -> e
  [(𝓒 b) b]
  [(𝓒 x) x]
  [(𝓒 (λ x e)) (λ x (𝓒 e))]
  [(𝓒 (grd j k v_κ v)) (grd j k (𝓒 v_κ) (𝓒 v))]
  [(𝓒 (flat v)) (flat (𝓒 v))]
  [(𝓒 (->i v_1 v_2)) (->i (𝓒 v_1) (𝓒 v_2))]
  [(𝓒 (>>t v_κ v_p))
   (multi
    (flat
     (λ x
       (*let ([x_α (queue)])
         ((𝓒 v_κ) e_c)))))
   (where e_c (flat
                (λ y
                  ((𝓒 v_p) (add! x_α y)))))]
  [(𝓒 (>>c v_α v_p))
   (flat
    (λ y
      ((𝓒 v_p) (add! (𝓒 v_α) y))))]
  [(𝓒 α) α]
  [(𝓒 l) l]
  [(𝓒 (>>t v_κ e_p))
   (multi
     (*let ([x_p (𝓒 e_p)])
       (multi
         (flat
           (λ x
             (*let ([x_α (queue)])
               ((𝓒 v_κ) e_c)))))))
   (where e_c (flat
                (λ y
                  (x_p (add! x_α y)))))]
  [(𝓒 (>>t e_κ e_p))
   (multi
     (*let ([x_κ (𝓒 e_κ)]
            [x_p (𝓒 e_p)])
       (multi
         (flat
           (λ x
             (*let ([x_α (queue)])
               (x_κ e_c)))))))
   (where e_c (flat
                (λ y
                  (x_p (add! x_α y)))))]
  [(𝓒 (o e)) (o (𝓒 e))]
  [(𝓒 (if e e_t e_f)) (if (𝓒 e) (𝓒 e_t) (𝓒 e_f))]
  [(𝓒 (e_f e_a)) ((𝓒 e_f) (𝓒 e_a))]
  [(𝓒 (queue)) (queue)]
  [(𝓒 (add! e_α e_v)) (add! (𝓒 e_α) (𝓒 e_v))]
  [(𝓒 (flat e)) (flat (𝓒 e))]
  [(𝓒 (->i e_d e_c)) (->i (𝓒 e_d) (𝓒 e_c))]
  [(𝓒 (>>t e_κ e_p)) (>>t (𝓒 e_κ) (𝓒 e_p))]
  [(𝓒 (mon j k l e_κ e_v)) (mon j k l (𝓒 e_κ) (𝓒 e_v))]
  [(𝓒 (mon j k e_κ e_v)) (mon j k (𝓒 e_κ) (𝓒 e_v))]
  [(𝓒 (err j k)) (err j k)])

(define-metafunction evaluation
  𝓔 : E -> E
  [(𝓔 hole) hole]
  [(𝓔 (o E)) (o (𝓔 E))]
  [(𝓔 (if E e_t e_f)) (if (𝓔 E) (𝓒 e_t) (𝓒 e_f))]
  [(𝓔 (E e)) ((𝓔 E) (𝓒 e))]
  [(𝓔 (v E)) ((𝓒 v) (𝓔 E))]
  [(𝓔 (add! E e)) (add! (𝓔 E) (𝓒 e))]
  [(𝓔 (add! v E)) (add! (𝓒 v) (𝓔 E))]
  [(𝓔 (flat E)) (flat (𝓔 E))]
  [(𝓔 (->i E e)) (->i (𝓔 E) (𝓒 e))]
  [(𝓔 (->i v E)) (->i (𝓒 v) (𝓔 E))]
  [(𝓔 (>>t E e_p))
   (multi
     (*let ([x_κ (𝓔 E)]
            [x_p (𝓒 e_p)])
       (multi
         (flat
           (λ x
             (*let ([x_α (queue)])
               (x_κ e_c)))))))
   (where e_c (flat
                (λ y
                  (x_p (add! x_α y)))))]
  [(𝓔 (>>t v_κ E))
   (multi
     (*let ([x_p (𝓔 E)])
       (multi
         (flat
          (λ x
            (*let ([x_α (queue)])
              ((𝓒 v_κ) e_c)))))))
   (where e_c (flat
                (λ y
                  (x_p (add! x_α y)))))]
  [(𝓔 (mon j k E e)) (mon j k (𝓔 E) (𝓒 e))]
  [(𝓔 (mon j k v E)) (mon j k (𝓒 v) (𝓔 E))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simulation judgment

(define-judgment-form evaluation
  #:mode (ς≈ I I)
  #:contract (ς≈ ς ς)
  [(ς≈ (~ e Σ) (~ e_° Σ_°))
   (Σ≈ Σ Σ_°)
   (side-condition
    ,(alpha-equivalent? evaluation (term (𝓒 e)) (term e_°)))])

(define-judgment-form evaluation
  #:mode (Σ≈ I I)
  #:contract (Σ≈ Σ Σ)
  [(Σ≈ () ())]
  [(Σ≈ ([α null] [α_* u_*] ...) Σ_°)
   (where null (lookup Σ_° α))
   (Σ≈ ([α_* u_*] ...) (rem Σ_° α))]
  [(Σ≈ ([α (cons v α_′)] [α_* u_*] ...) Σ_°)
   (where (cons v_° α_′) (lookup Σ_° α))
   (side-condition
    ,(alpha-equivalent? evaluation (term (𝓒 v)) (term v_°)))
   (Σ≈ ([α_* u_*] ...) (rem Σ_° α))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; homomorphism judgment

(define-judgment-form evaluation
  #:mode (𝓔≉ I)
  #:contract (𝓔≉ e)
  [(𝓔≉ (in-hole E e_0))
   (where e_* (𝓒 (in-hole E e_0)))
   (where e_° (in-hole (𝓔 E) (𝓒 e_0)))
   (side-condition
    ,(not (redex-match? evaluation v (term e_0))))
   (side-condition
    ,(not (alpha-equivalent? evaluation (term e_*) (term e_°))))])
