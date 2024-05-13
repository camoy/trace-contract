#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide balance-ok?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes and mixins

;; Functional encoding of a bank account class with support for deposit,
;; withdraw, and total methods.
(define (account% balance)
  (λ (msg . args)
    (case msg
      [(deposit) (set! balance (+ balance (car args)))]
      [(withdraw) (set! balance (- balance (car args)))]
      [(total) balance]
      [else (error "no method")])))

;; Mixin for accounts that adds a pretty method for getting a pretty printed
;; representation of the total.
(define ((pretty-mixin %) initial)
  (define internal-obj (% initial))
  (λ (msg . args)
    (case msg
      [(pretty)
       (format "You have $~a left in your account."
               (internal-obj 'total))]
      [else (apply internal-obj msg args)])))

;; Mixin that skims off a dollar when you deposit.
(define ((evil-mixin %) initial)
  (define internal-obj (% initial))
  (λ (msg . args)
    (case msg
      [(deposit) (internal-obj 'deposit (sub1 (car args)))]
      [else (apply internal-obj msg args)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Given a sequence of opeations, ensures that the account total is balanced.
;; In other words, when a total is reported, it is exactly:
;;   total = initial + deposit - withdraw
(define (balance-ok? t)
  (for/fold ([bal 0])
            ([m t])
    #:break (not bal)
    (match-define (list msg arg ret) m)
    (case msg
      [(deposit) (+ bal arg)]
      [(withdraw) (- bal arg)]
      [(total) (and (= bal ret) bal)]
      ;; Ignore other messages (needed since the mixin adds a new method).
      [else bal])))

;; For the account% class. Ensures that every account created is balanced.
(define account/c
  (trace/c ([sent any/c])
    (-> (list/t 'deposit sent (void))
        (->i ([msg symbol?]
              [arg any/c])
             [ret (msg arg) (list/t msg arg sent)]))
    (full (sent) balance-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define/contract account+c% account/c account%)
   #:do (define acc (account+c% 100))
   #:do (acc 'deposit 42)
   #:do (acc 'withdraw 20)
   (acc 'total (void)) 122

   #:do (define/contract pretty-account+c% account/c (pretty-mixin account%))
   #:do (define acc* (pretty-account+c% 100))
   #:do (acc* 'deposit 42)
   #:do (acc* 'withdraw 20)
   (acc* 'total (void)) 122
   (acc* 'pretty (void)) "You have $122 left in your account."

   #:do (define/contract evil-account+c% account/c (evil-mixin account%))
   #:do (define acc** (evil-account+c% 100))
   (acc** 'total (void)) 100
   #:do (acc** 'deposit 10)
   #:x
   (acc** 'total (void))
   (trace-exn? '(definition evil-account+c%)
               (current-contract-region)
               "produced: '(total #<void> 109)")
   ))
