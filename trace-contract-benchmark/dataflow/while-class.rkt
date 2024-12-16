#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide while%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/set
         redex-etc
         redex/reduction-semantics
         "interface.rkt"
         "while.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define while%
  (class* object% (language<%>)
    (super-new)

    (define/public (init stmt)
      (match-term While stmt
       [{(:= X A) ℓ}
        (term ℓ)]
       [{skip ℓ}
        (term ℓ)]
       [(seq S_0 S ...)
        (init (term S_0))]
       [(if {B ℓ} S_0 S_1)
        (term ℓ)]
       [(while {B ℓ} S)
        (term ℓ)]))

    (define/public (final stmt)
       (match-term While stmt
        [{(:= X A) ℓ}
         (set (term ℓ))]
        [{skip ℓ}
         (set (term ℓ))]
        [(seq S_0 ... S)
         (final (term S))]
        [(if {B ℓ} S_0 S_1)
         (set-union (final (term S_0)) (final (term S_1)))]
        [(while {B ℓ} S)
         (set (term ℓ))]))

    (define/public (blocks stmt)
       (match-term While stmt
        [{(:= X A) ℓ}  (set stmt)]
        [{skip ℓ}      (set stmt)]
        [(seq S_0 S_1 ...)
         (apply set-union
                (for/list ([t (in-list (term (S_0 S_1 ...)))])
                  (blocks t)))]
        [(if {B ℓ} S_0 S_1)
         (set-union (set (term {B ℓ}))
                    (blocks (term S_0)) (blocks (term S_1)))]
        [(while {B ℓ} S)
         (set-union (set (term {B ℓ}))
                    (blocks (term S)))]))

    (define/public (labels stmt)
       (for/set ([block (in-set (blocks stmt))])
         (match-term While block
           [{_ ℓ} (term ℓ)])))

    (define/public (flow stmt)
       (match-term While stmt
        [{(:= X A) ℓ} (set)]
        [{skip ℓ}     (set)]
        [(seq S)     (flow (term S))]
        [(seq S_0 S_1 S ...)
         (set-union (flow (term S_0))
                    (flow (term S_1))
                    (for/set ([ℓ (in-set (final (term S_0)))])
                      (cons ℓ (init (term S_1))))
                    (flow (term (seq S_1 S ...))))]
        [(if (name b {B ℓ}) S_0 S_1)
         (set-union (flow (term S_0))
                    (flow (term S_1))
                    (set (cons (term ℓ) (init (term S_0)))
                         (cons (term ℓ) (init (term S_1)))))]
        [(while (name b {B ℓ}) S)
         (set-union (flow (term S))
                    (set  (cons (term ℓ) (init (term S))))
                    (for/set ([ℓ* (in-set (final (term S)))])
                      (cons ℓ* (term ℓ))))]))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#;(module+ test
  (require chk)

  (define while-obj (new while%))

  (define-term fact-5
    (seq
     {(:= x 5) ℓ1}
     {(:= y 1) ℓ2}
     {(:= z 1) ℓ3}
     (while {(< z x) ℓ4}
       (seq
        {(:= z (+ z 1)) ℓ5}
        {(:= y (* y z)) ℓ6}))))

  (define-term nested
    (seq {(:= x 1) ℓ1}
         (if {(= x 1) ℓ2}
             {skip ℓ3}
             (if {(= x 2) ℓ4}
                 {skip ℓ5}
                 {skip ℓ6}))))

  (with-method ([init   (while-obj init)]
                [final  (while-obj final)]
                [blocks (while-obj blocks)]
                [labels (while-obj labels)]
                [flow   (while-obj flow)])
    (chk
     (init (term fact-5)) 'ℓ1
     (init (term nested)) 'ℓ1

     (final (term fact-5))
     (list->set '(ℓ4))

     (final (term nested))
     (list->set '(ℓ3 ℓ5 ℓ6))

     (blocks (term fact-5))
     (list->set
      '({(:= x 5) ℓ1}
        {(:= y 1) ℓ2}
        {(:= z 1) ℓ3}
        {(< z x) ℓ4}
        {(:= z (+ z 1)) ℓ5}
        {(:= y (* y z)) ℓ6}))

     (blocks (term nested))
     (list->set
      '({(:= x 1) ℓ1}
        {(= x 1) ℓ2}
        {skip ℓ3}
        {(= x 2) ℓ4}
        {skip ℓ5}
        {skip ℓ6}))

     (labels (term fact-5))
     (list->set '(ℓ1 ℓ2 ℓ3 ℓ4 ℓ5 ℓ6))

     (labels (term nested))
     (list->set '(ℓ1 ℓ2 ℓ3 ℓ4 ℓ5 ℓ6))

     (flow (term fact-5))
     (list->set '((ℓ1 . ℓ2) (ℓ2 . ℓ3) (ℓ3 . ℓ4) (ℓ4 . ℓ5) (ℓ5 . ℓ6) (ℓ6 . ℓ4)))

     (flow (term nested))
     (list->set '((ℓ1 . ℓ2) (ℓ2 . ℓ3) (ℓ2 . ℓ4) (ℓ4 . ℓ5) (ℓ4 . ℓ6)))
     )))
