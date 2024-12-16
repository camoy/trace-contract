#lang racket

(provide
 ;; DEFINITION SYNTAX
 #; (define-enumeration name:id [s:id e:expr] ...)
 ;; generates definitions for `s` ..., initialized to `e` ..., respectively
 ;; plus `name`, which is a way to check what an ennumeration provides as a list
 ;; plus `name?`, which is a predicate for these values 
 #; (define-enumeration name:id s:id ...)
 ;; like above, but each `s` is just a gensym-ed string mentioning `s`
 define-enumeration)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket ~a)))
(module+ test
  (require syntax/macro-testing)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (define-enumeration stx)
  (syntax-parse stx
    [[_ name:id [s:id e:expr] ...]
     #:fail-when (equal? 'expression (syntax-local-context))
     "define-enumeration must be in a definition context"
     #:with name? (predicate? #'name)
     (syntax/loc stx
       (begin (define s e) ...
              (define name `[[s ,s] ...])
              (define (name? x) (cons? (member x (map second name))))))]
    [[_ name:id s:id ...]
     #:fail-when (equal? 'expression (syntax-local-context))
     "define-enumeration must be in a definition context"
     (syntax/loc stx (define-enumeration name [s (~a (gensym 's))] ...))]))

(define-for-syntax (predicate? name)
  (datum->syntax name (string->symbol (~a (syntax-e name) "?")) name name))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (let ()
                  (define-enumeration A [x 1] [y 2])
                  A)
                '[[x 1] [y 2]]
              "'the whole enumeration' test")
  
  (check-true (let ()
                (define-enumeration A x y z)
                (A? x))
              "predicate true test")
  (check-false (let ()
                (define-enumeration A x y z)
                (A? 0))
              "predicate false test")

  (check-exn #px"definition context"
             (λ ()
               (convert-compile-time-error
                (let ([x (define-enumeration A x y z)])
                  x)))
             "catches expression context"))

;; ---------------------------------------------------------------------------------------------------
;; TODO

#;
(define-syntax (sym-case stx)
  (syntax-case stx ()
    [(_ id val-expr [(sym) expr] ...)
     (let ()
       (define expected-ids
         (syntax-local-value
          #'id
          (λ ()
            (raise-syntax-error
             'sym-case
             "expected an identifier bound via define-sym-case"
             stx
             #'id))))
       (define actual-ids (syntax->datum #'(sym ...)))
       (unless (equal? expected-ids actual-ids)
         (raise-syntax-error
          'sym-case
          (format "expected the symbols ~s"
                  expected-ids)
          stx))
       #'(case val-expr [(sym) expr] ...))]))

