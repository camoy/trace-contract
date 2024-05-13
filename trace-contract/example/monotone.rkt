#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data/order
         racket/bool
         racket/dict
         "util/red-black-tree.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Determines if the trace represents a monotonic function. We use an ordered
;; dictionary to check the two immediately neighboring points only.
(define/match (monotone-fold acc x+y)
  [(t (list x y))
   (define pred-y (dict-pred t x))
   (define succ-y (dict-succ t x))
   (if (and (implies pred-y (<= pred-y y))
            (implies succ-y (<= y succ-y)))
       (dict-set t x y)
       (fail))])

;; Contract for monotonicity using fold.
(define monotone-fast/c
  (trace/c ([x+y any/c])
    (->i ([x real?])
         [y (x) (and/c real? (list/t x x+y))])
    (accumulate (red-black-tree)
     [(x+y) monotone-fold])))

;; Returns if the given input and output lists represent a monotone function.
(define (monotone? x y)
  (for*/and ([(x1 y1) (in-parallel x y)]
             [(x2 y2) (in-parallel x y)])
    (implies (<= x1 x2) (<= y1 y2))))

;; Contract for monotonicity in an inefficient way.
(define monotone-slow/c
  (trace/c ([x real?] [y real?])
    (-> x y)
    (full (x y) monotone?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dictionary operations

(define (dict-pred d x)
  (define iter (dict-iterate-greatest/<=? d x))
  (and iter (dict-iterate-value d iter)))

(define (dict-succ d x)
  (define iter (dict-iterate-least/>=? d x))
  (and iter (dict-iterate-value d iter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests

(module+ test
  (require chk
           racket/math)

  (define (chk-monotone monotone/c error-msg)
    (chk
     #:do (define/contract add1* monotone/c add1)
     (add1* -2) -1
     (add1* -1) 0
     (add1* 0) 1
     (add1* 1) 2

     #:do (define/contract sqr* monotone/c sqr)
     (sqr* 0) 0
     (sqr* 1) 1
     (sqr* 2) 4
     #:x
     (sqr* -1)
     (trace-exn? '(definition sqr*)
                 (current-contract-region)
                 error-msg)
     ))

  (chk-monotone monotone-fast/c "produced: '(-1 1)")
  (chk-monotone monotone-slow/c "x: (0 1 2 -1)"))
