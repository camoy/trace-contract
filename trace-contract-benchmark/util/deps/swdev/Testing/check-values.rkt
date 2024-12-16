#lang racket

(provide
 ;; SYNTAX
 #;(check-values actual:expr expected:expr ... msg:string)
 ;; evaluate actual to as many values as expected (lengyth (expected ...))
 ;; then compare values one by one
 check-values)

(require rackunit)
(require (for-syntax syntax/parse))

(define-syntax (check-values stx)
  (syntax-parse stx 
    [(_ actual expected ... msg)
     #:do [(define n (length (syntax->list #'(expected ...))))]
     #:with r (datum->syntax #f (format "~a values expected, but not received" n))
     #:with (temp ...) (generate-temporaries #'(expected ...))
     (quasisyntax/loc stx
       (let ()
         (define n (length '(expected ...)))
         (define x (call-with-values (λ () actual) list))
         (check-equal? (length x) (length '(temp ...)) (~a n " values expected, but not received"))
         (unless (= (length x) (length '(temp ...)))
           (check-equal? x (list expected ...)))
         (when (= (length x) (length '(temp ...)))
           (define-values (temp ...) (apply values x))
           (check-equal? temp expected msg)
           ...)))]))

(module+ test 
  (check-values (values 1 2 3) 1 2 "hello")
  (check-values (values 1 2 3) 1 2 3 "hello"))