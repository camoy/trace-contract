#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-etc
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (sequence-ok? t)
  (>= (stream-fold + 0 t) 0))

;; For acquiring and releasing a re-entrant lock.
(define (make-contracts)
  (trace/c ([t (or/c (=/c -1) (=/c 1))])
    #:global
    (values (apply/c [t 1])
            (apply/c [t -1]))
    (full (t) sequence-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/strip-context)
           chk)

  (define-syntax (define-acquire-release stx)
    (replace-context
     stx
     #'(define-values/contract (acquire release)
         (make-contracts)
         (values void void))))

  (chk
   #:t
   (begin
     (define-acquire-release)
     (acquire)
     (release)
     (acquire))
   #:t
   (begin
     (define-acquire-release)
     (acquire)
     (acquire)
     (release)
     (acquire)
     (release)
     (release))
   #:x
   (begin
     (define-acquire-release)
     (acquire)
     (release)
     (acquire)
     (release)
     (release))
   (trace-exn? (current-contract-region)
               '(definition release)
               "t: (1 -1 1 -1 -1)")
   ))
