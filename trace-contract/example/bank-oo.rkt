#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         "bank.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes and mixins

;; A bank account class with support for deposit, withdraw, and total methods.
(define account%
  (class object%
    (init init-balance)
    (define balance init-balance)
    (super-new)

    (define/public (deposit x)
      (set! balance (+ balance x)))

    (define/public (withdraw x)
      (set! balance (- balance x)))

    (define/public (total)
      balance)
    ))

;; Mixin for accounts that adds a pretty method for getting a pretty printed
;; representation of the total.
(define (pretty-mixin %)
  (class %
    (super-new)
    (inherit total)

    (define/public (pretty)
      (format "You have $~a left in your account."
              (total)))
    ))

;; Mixin that skims off a dollar when you deposit.
(define (evil-mixin %)
  (class %
    (super-new)

    (define/override (deposit x)
      (super deposit (sub1 x)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; For the account% class. Ensures that every account created is balanced. We
;; are even able to reuse `balance-ok?` from the function encoding.
(define account/c
  (trace/c ([sent any/c])
    (class/c
     [deposit (->m (list/t 'deposit sent (void)) any)]
     [withdraw (->m (list/t 'withdraw sent (void)) any)]
     [total (->m (list/t 'total (void) sent))]
     [init (init-balance (list/t 'deposit sent (void)))])
    (full (sent) balance-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:do (define/contract account+c% account/c account%)
   #:do (define acc (new account+c% [init-balance 100]))
   #:do (send acc deposit 42)
   #:do (send acc withdraw 20)
   (send acc total) 122

   #:do (define/contract pretty-account+c% account/c (pretty-mixin account%))
   #:do (define acc* (new pretty-account+c% [init-balance 100]))
   #:do (send acc* deposit 42)
   #:do (send acc* withdraw 20)
   (send acc* total) 122
   (send acc* pretty) "You have $122 left in your account."

   #:do (define/contract evil-account+c% account/c (evil-mixin account%))
   #:do (define acc** (new evil-account+c% [init-balance 100]))
   (send acc** total) 100
   #:do (send acc** deposit 10)
   #:x
   (send acc** total)
   (trace-exn? '(definition evil-account+c%)
               (current-contract-region)
               "produced: '(total #<void> 109)")
  ))
