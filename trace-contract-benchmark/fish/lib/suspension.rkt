#lang racket

(provide
 ;; SYNTAX
 #; (suspension e1 e2 ...)
 ;; creates a function of no arguments that always returns the result of the first call
 ;; any side effects of e1 e2 ... are only observable during the extent of the first call
 suspend)

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (suspend e1 e2 ...) (suspension (λ () e1 e2 ...) #false))

(struct suspension [thunk-x x]
  #:transparent
  #:mutable
  #:property prop:procedure
  (λ (self)
    (define is-thunk (suspension-thunk-x self))
    (when is-thunk
      (set-suspension-x! self (is-thunk))
      (set-suspension-thunk-x! self #false))
    (suspension-x self)))
