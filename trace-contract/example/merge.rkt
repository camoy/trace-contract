#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Stream that enforces that inputs are followed by outputs that increase which
;; add one to the input.
(define (adding? x y)
  (define st (trace-merge x y))
  (let go ([st st] [n 1])
    (match st
      [(stream) #t]
      [(stream `(,(== n) in)) #t]
      [(stream* `(,(== n) in) `(,(== (add1 n)) out) st-rst)
       (go st-rst (+ n 2))]
      [_ #f])))

;; For a function whose that goes back and forth between input and output, where
;; the output adds one to the input.
(define adding/c
  (trace/c ([x integer?]
            [y integer?])
    (-> (list/t x 'in)
        (list/t y 'out))
    (full (x y) adding?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do
   (define/contract add1*
     adding/c
     add1)

   (list (add1* 1) (add1* 3))
   '(2 4)

   #:x
   (add1* 4)
   (trace-exn? (current-contract-region)
               '(definition add1*)
               "given: '(4 in)")
   ))
