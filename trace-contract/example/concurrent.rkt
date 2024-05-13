#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (folder acc pr)
  (match-define (list who what) pr)
  (match-define (list acc-left acc-right) acc)
  (match who
    ['left
     #:when (= (add1 acc-left) what)
     (list what acc-right)]
    ['right
     #:when (= (add1 acc-right) what)
     (list acc-left what)]
    [_ (fail)]))

(define ctc
  (trace/c ([x integer?])
    (cons/c (-> (list/t 'left x) any)
            (-> (list/t 'right x) any))
    (accumulate (list -1 -1)
      [(x) folder])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract fg ctc (cons values values))
   #:do (match-define (cons f g) fg)
   #:do (define f-exn? #f)
   #:do (define thread-f
          (thread
           (λ ()
             (with-handlers ([exn? (λ _ (set! f-exn? #t))])
               (for ([k (in-range 10000)])
                 (f k))))))
   #:do (define g-exn? #f)
   #:do (define thread-g
          (thread
           (λ ()
             (with-handlers ([exn? (λ _ (set! g-exn? #t))])
               (for ([k (in-range 10000)])
                 (g k))))))

   #:do
   (begin (thread-wait thread-f)
          (thread-wait thread-g))
   #:! (or f-exn? g-exn?)
   ))
