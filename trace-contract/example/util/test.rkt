#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide (all-from-out
          racket/contract
          racket/function
          racket/list
          racket/match
          racket/set
          racket/stream
          stream-etc
	  trace-contract
          #;"../../main.rkt")
         define-values/contract
         trace-exn?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         racket/list
         racket/match
         racket/set
         racket/stream
         racket/string
         stream-etc
         trace-contract
         #;"../../main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

;; Ensures that the given exception contains the given strings.
(define ((trace-exn? pos-party neg-party . strs) e)
  (define blm (exn:fail:contract:blame-object e))
  (and (equal? (blame-positive blm) pos-party)
       (equal? (blame-negative blm) neg-party)
       (for/and ([s (in-list strs)])
         (string-contains? (exn-message e) s))))

;; Helper for attaching multiple contracts to multiple values.
(define-syntax (define-values/contract stx)
  (syntax-parse stx
    [(_ (name:id ...) ctc:expr body:expr)
     #:with (ctc-name ...) (generate-temporaries #'(name ...))
     #:with (body-name ...) (generate-temporaries #'(name ...))
     #'(begin
         (define-values (ctc-name ...) ctc)
         (define-values (body-name ...) body)
         (define/contract name ctc-name body-name) ...)]))
