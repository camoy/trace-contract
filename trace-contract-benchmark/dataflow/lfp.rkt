#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide LFP%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/list
         racket/match
         racket/set
         "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

;; Monotone-Framework → [Hash Label L] [Hash Label L]
;; Takes an instance of a monotone framework and returns the minimal fixed point
;; solution of the set of equations defined by the lattice for both entry and
;; exit blocks.
(define LFP%
  (class* object% (analysis<%>)
    (init framework)
    (define-values (L F E ι f)
      (values (send framework L) (send framework F)
              (send framework E) (send framework ι)
              (λ xs (send/apply framework f xs))))
    (super-new)

    (define initial-analysis
      (with-method ([⊥ (L ⊥)])
        (for/hash ([ℓ (in-set (set-union (flow->labels F) E))])
          (if (set-member? E ℓ)
              (values ℓ ι)
              (values ℓ (⊥))))))

    (define/public (entry-facts)
      (with-method ([⊑ (L ⊑)]
                    [⊔ (L ⊔)])
        (let go ([worklist (set->list F)]
                 [analysis initial-analysis])
          (if (empty? worklist)
              analysis
              (match-let* ([(cons ℓ ℓ*) (first worklist)]
                           [worklist*   (rest  worklist)]
                           [f-analysisℓ (f ℓ (hash-ref analysis ℓ))]
                           [analysisℓ*  (hash-ref analysis ℓ*)])
                (if (not (⊑ f-analysisℓ analysisℓ*))
                    (go (append (flow-from F ℓ*) worklist*)
                        (hash-set analysis ℓ* (⊔ analysisℓ* f-analysisℓ)))
                    (go worklist* analysis)))))))

    (define/public (exit-facts)
      (for/hash ([(ℓ facts) (in-hash (entry-facts))])
        (values ℓ (f ℓ facts))))
    ))

;; [Set [Pair Label Label]] → [Set Label]
;; Returns the set of labels induced by the flow.
(define (flow->labels flow)
  (set-union (for/set ([e (in-set flow)])
               (car e))
             (for/set ([e (in-set flow)])
               (cdr e))))

;; [Set [Pair Label Label]] Label → [List [Pair Label Label]]
;; Returns the list of edges that flow from a source label.
(define (flow-from flow source)
  (for/list ([edge (in-set flow)]
            #:when (equal? (car edge) source))
    edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(module+ test
  (require chk)

  (chk
   (flow->labels (list->set '((ℓ1 . ℓ2) (ℓ2 . ℓ3))))
   (set 'ℓ1 'ℓ2 'ℓ3)

   #:eq set=?
   (flow-from (list->set '((ℓ1 . ℓ2) (ℓ1 . ℓ3))) 'ℓ1)
   '((ℓ1 . ℓ2) (ℓ1 . ℓ3))

   (flow-from (set '(ℓ1 . ℓ2)) 'ℓ2)
   '()
   ))
