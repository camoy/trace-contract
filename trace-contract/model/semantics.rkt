#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; abbreviations
 λ*
 *let
 seq
 ->
 %
 multi

 ;; reduction relation
 ↦

 ;; metafunctions
 δ
 res->ans
 next-addr
 queue-add)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require redex/reduction-semantics
         redex/pict
         (except-in redex-etc not-match?)
         racket/function
         racket/list
         racket/hash
         racket/match
         racket/set
         "syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbreviations

(define-metafunction surface
  λ* : (any any ...) any -> any
  [(λ* (any) any_e) (λ any any_e)]
  [(λ* (any_1 any_2 ...) any_e) (λ any_1 (λ* (any_2 ...) any_e))])

(define-metafunction surface
  *let : ([x any] ...) any -> any
  [(*let () any_b) any_b]
  [(*let ([x_1 any_1] [x_2 any_2] ...) any_b)
   ((λ x_1 (*let ([x_2 any_2] ...) any_b)) any_1)])

(define-metafunction surface
  seq : any ... any -> any
  [(seq any_1 ... any_2)
   (*let ([x any_1] ...) any_2)
   (where (x ...) ,(map (λ _ (gensym '_)) (term (any_1 ...))))])

(define-metafunction surface
  -> : any any -> any
  [(-> any_d any_c) (->i any_d (λ ,(gensym 'x) any_c))])

(define-metafunction evaluation
  % : any -> any
  [(% any) any])

(define-metafunction evaluation
  multi : any -> any
  [(multi any) any])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduction relation

(define ↦
  (reduction-relation
   evaluation
   #:arrow :->
   #:domain ς
   #:codomain ς

   [:-> (~ (in-hole E (mon j k (>>t v_κ v_p) v)) Σ)
        (~ (in-hole E (mon j k (% (v_κ (>>c α v_p))) v)) Σ_′)
        (side-condition 'newline)
        (where α (next-addr Σ))
        (where Σ_′ (ext Σ [α null]))
        Mon-Trace]

   [:-> (~ (in-hole E (mon j k (>>c α v_p) v)) Σ)
        (~ (in-hole E (mon j k (% (v_p α)) v)) Σ_′)
        (where Σ_′ (queue-add Σ α v))
        Mon-Col]

   [:-> (~ (in-hole E (mon j k l e_κ e)) Σ)
        (~ (in-hole E ((% (mon j k e_κ e)) l)) Σ)
        Mon-Apply]

   [:-> (~ (in-hole E (mon j k v_κ v)) Σ)
        (~ (err "Λ" j) Σ)
        (side-condition (not-match-set? evaluation κ v_κ))
        Err-Mon]

   [:-> (~ (in-hole E (mon j k true v)) Σ)
        (~ (in-hole E (grd j k true v)) Σ)
        Mon-True]

   [:-> (~ (in-hole E ((side-condition (grd j k true v) '%) l)) Σ)
        #;(~ (in-hole E ((grd j k true v) l)) Σ)
        (~ (in-hole E v) Σ)
        Grd-True]

   [:-> (~ (in-hole E (mon j k false v)) Σ)
        (~ (err j k) Σ)
        Err-False]

   [:-> (~ (in-hole E (mon j k (flat v_p) v)) Σ)
        (~ (in-hole E (mon j k (% (v_p v)) v)) Σ)
        Mon-Flat]

   [:-> (~ (in-hole E (mon j k (->i v_d v_c) (λ x e))) Σ)
        (~ (in-hole E (grd j k (->i v_d v_c) (λ x e))) Σ)
        Mon-Arr]

   [:-> (~ (in-hole E (mon j k (->i v_d v_c) v)) Σ)
        (~ (err j k) Σ)
        (side-condition (not-match? evaluation (λ x e) v))
        Err-Arr]

   [:-> (~ (in-hole E ((side-condition (grd j k (->i v_d v_c) v) '%) l)) Σ)
        #;(~ (in-hole E ((grd j k (->i v_d v_c) v) l)) Σ)
        (multi
         (~ (in-hole E (λ x
                         (*let ([x_m (mon j l v_d x)])
                           (mon j k l
                                (% (v_c (% (x_m j))))
                                (% (v (% (x_m k))))))))
            Σ))
        (fresh x x_m)
        Grd-Arr]

   [:-> (~ (in-hole E (queue)) Σ)
        (~ (in-hole E α) Σ_′)
        (where α (next-addr Σ))
        (where Σ_′ (ext Σ [α null]))
        Queue]

   [:-> (~ (in-hole E (add! α v)) Σ)
        (~ (in-hole E α) Σ_′)
        (where Σ_′ (queue-add Σ α v))
        Add!]

   [:-> (~ (in-hole E (add! v_q v)) Σ)
        (~ (err "Λ" "†") Σ)
        (side-condition (not-match-set? evaluation α v_q))
        Err-Add!]

   [:-> (~ (in-hole E (o v)) Σ)
        (~ (in-hole E v_′) Σ)
        (where v_′ (δ o (lookup* Σ v)))
        Prim]

   [:-> (~ (in-hole E (o v)) Σ)
        (~ (err j k) Σ)
        (where (err j k) (δ o (lookup* Σ v)))
        Err-Prim]

   [:-> (~ (in-hole E (if v e_1 e_2)) Σ)
        (~ (in-hole E e_1) Σ)
        (side-condition (not-match? evaluation false v))
        If-True]

   [:-> (~ (in-hole E (if false e_1 e_2)) Σ)
        (~ (in-hole E e_2) Σ)
        If-False]

   [:-> (~ (in-hole E ((λ x e) v)) Σ)
        (~ (in-hole E (substitute e x v)) Σ)
        App]

   [:-> (~ (in-hole E (v_f v)) Σ)
        (~ (err "Λ" "†") Σ)
        (side-condition (not-match-set? evaluation f v_f))
        Err-App]))

(define-syntax-rule (not-match? _ pat e)
  (not (redex-match? evaluation pat (term e))))

(define-syntax-rule (not-match-set? _ pat e)
  (not (redex-match? evaluation pat (term e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunctions

(define-metafunction evaluation
  δ : o w -> r
  [(δ null? null) true]
  [(δ null? _) false]
  [(δ head (cons v α_′)) v]
  [(δ tail (cons v α_′)) α_′]
  [(δ o _) (err "Λ" "†")])

(define-metafunction evaluation
  lookup* : Σ v -> w
  [(lookup* Σ α) (lookup Σ α)]
  [(lookup* Σ v) v])

(define-metafunction evaluation
  res->ans : r -> a
  [(res->ans b) b]
  [(res->ans v) "opaque"]
  [(res->ans (err j k)) (err j k)])

(define-metafunction evaluation
  next-addr : Σ -> α
  [(next-addr Σ) (next-addr* Σ)])

(define-metafunction evaluation
  next-addr* : Σ -> α
  [(next-addr* ([α u] ...))
   ,(add1 (apply max -1 (term (α ...))))])

(define-metafunction evaluation
  queue-add : Σ α v -> Σ
  [(queue-add Σ α v)
   (ext Σ [α (cons v α_′)] [α_′ null])
   (where null (lookup Σ α))
   (where α_′ (next-addr Σ))]
  [(queue-add Σ α v)
   (queue-add Σ α_′ v)
   (where (cons v_h α_′) (lookup Σ α))])
