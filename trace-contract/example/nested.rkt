#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (ok? st)
  (let go ([st st] [b 0] [c 0])
    (match st
      [(stream) #t]
      [(stream* `(,n a) rst)
       (and (>= n (+ b c)) (go rst b c))]
      [(stream* `(,n b) rst) (go rst (+ b n) c)]
      [(stream* `(,n c) rst) (go rst b (+ c n))])))

(define con
  (trace/c ([x integer?])
    (-> (list/t x 'a)
        (trace/c ([y integer?] [z integer?])
          (-> (list/t y 'b)
              (list/t z 'c))
          (full (x y z) (compose ok? trace-merge))))
    (full (x) (λ _ #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do
   (define/contract f
     con
     (λ (x) (λ (y) y)))
   #:do (define f1 (f 1))
   (f1 2)  2
   #:do (define f2 (f 4))
   (f2 2)  2
   #:x (f 3)
   "given: '(3 a)"
   ))
