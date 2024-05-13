#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (mono-fold acc x y)
  (match-define (list xs ys) acc)
  (define xs* (append xs (list x)))
  (define ys* (append ys (list y)))
  (if (equal? xs* ys*) (list xs* ys*) (fail)))

(define all-fold/c
  (trace/c ([x real?] [y real?])
    (-> x y)
    (accumulate '(() ())
     [(x y) mono-fold])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests

(module+ test
  (require chk)

  (define prompt (make-continuation-prompt-tag))

  (chk
   #:do (define/contract values** all-fold/c values)
   (values** -2) -2
   (values** 1) 1

   #:do (define/contract esc*
          all-fold/c
          (λ (x)
            (if (negative? x)
                (abort-current-continuation prompt x)
                x)))
   (call-with-continuation-prompt esc* prompt values -2) -2
   (esc* 1) 1
   ))
