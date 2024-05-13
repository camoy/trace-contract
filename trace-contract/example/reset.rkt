#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (folder-default acc y)
  (define acc* (+ acc y))
  (if (negative? acc*)
      (fail)
      acc*))

;; Uses a `#:fold` clause with the default behavior.
(define sum-fold-default/c
  (trace/c ([y integer?])
    (-> integer? y)
    (accumulate 0 [(y) folder-default])))

(define (folder-continue acc y)
  (define acc* (+ acc y))
  (if (negative? acc*)
      (fail #:reset acc*)
      acc*))

;; The same as above with "continue" behavior.
(define sum-fold-continue/c
  (trace/c ([y integer?])
    (-> integer? y)
    (accumulate 0 [(y) folder-continue])))

(define (folder-reset acc y)
  (define acc* (+ acc y))
  (if (negative? acc*)
      (fail #:reset 0)
      acc*))

;; The same as above with "reset" behavior.
(define sum-fold-reset/c
  (trace/c ([y integer?])
    (-> integer? y)
    (accumulate 0 [(y) folder-reset])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define (chk-continue sum/c)
    (chk
     #:do (define/contract values* sum/c values)

     (list (values* 1) (values* -1) (values* 1) (values* -1))
     '(1 -1 1 -1)

     #:x
     (values* -5)
     (trace-exn? '(definition values*)
                 (current-contract-region)
                 "produced: -5")

     #:x
     (values* 1)
     (trace-exn? '(definition values*)
                 (current-contract-region)
                 "produced: 1")
     ))

  (define (chk-reset sum/c err-str)
    (chk
     #:do (define/contract values* sum/c values)

     (list (values* 1) (values* -1) (values* 1) (values* -1))
     '(1 -1 1 -1)

     #:x
     (values* -1)
     (trace-exn? '(definition values*)
                 (current-contract-region)
                 err-str)

     (list (values* 1) (values* -1) (values* 1) (values* -1))
     '(1 -1 1 -1)

     #:x
     (values* -1)
     (trace-exn? '(definition values*)
                 (current-contract-region)
                 err-str)
     ))

  (chk-reset sum-fold-default/c "produced: -1")
  (chk-continue sum-fold-continue/c)
  (chk-reset sum-fold-reset/c "produced: -1")
  )
