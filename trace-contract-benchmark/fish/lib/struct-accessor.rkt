#lang racket

(provide
 ;; SYNTAX
 #; (struct-accessor struct-name field-name)
 ;; produces a function that accesses `field-name` of an instande of `struct-name`
 struct-accessor)

;; -----------------------------------------------------------------------------
(require (for-syntax syntax/parse racket/struct-info racket/list))

;; -----------------------------------------------------------------------------
(define-syntax struct-accessor
  (syntax-parser
    [(_ s-name:id f-name:id s)
     #'[(struct-accessor s-name f-name) s]]
    [(_ s-name:id f-name:id)
     (define info      (syntax-local-value #'s-name))
     (define the-accessor
       (for/first ([f (struct-field-info-list info)]
                   [a (fourth (extract-struct-info info))]
                   #:when (eq? (syntax-e #'f-name) f)) a))
     (define me (syntax-e (car (syntax->list this-syntax))))
     (unless the-accessor
       (raise-syntax-error me "field does not exist" #'s-name #'f-name))
     the-accessor]))
