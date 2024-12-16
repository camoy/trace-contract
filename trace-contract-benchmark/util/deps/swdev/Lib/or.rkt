#lang racket

;; an or form that supports a kind of local definition of identifiers and values 

(provide
 ;; SYNTAX
 #; {e = ... (or~ {e or #:let x e}*)}
 ;; a #:let x e phrase evaluates e, binds the value to x, and
 ;; evaluates the rest of the sub-expressions in this lexical context
 or~

 ;; SYNTAX
 #; [e = ... (list/dc [x:id ctc:expr] ... {#:post name:id (y:id ...) c:expr ...})]
 ;; evaluate to a contract that first checks a list for its elements and then
 ;; the post condition involviing a subset of the labeled fields
 ;; WARNING: not robust
 list/dc)

;; -----------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(define-syntax (or~ stx)
  (syntax-parse stx
    [(_)
     #'#f]
    [(_ (~datum #:let) (~var x id) (~var rhs expr) rst ...)
     #'(let ([x rhs]) (or~ rst ...))]
    [(_ e1 e2 ...)
     #'(or e1 (or~ e2 ...))]))

(module+ test 
  (check-equal? (or~) (or))
  (check-equal? (or~ #:let x 1 x) (or 1))
  (check-equal? (or~ #f #:let x #f x) (or #f #f))
  (check-equal? (or~ #f #:let x 1 x #f) (or #f 1 #f)))

(define-syntax (list/dc stx)
  (syntax-parse stx
    [(_ [x:id c:expr] ... (~optional [~seq #:post name (y:id ...) prop:expr ...]))
     #:declare name (expr/c #'string?)
     ;; check that y ... < x ...
     #'(and/c
        (list/c c ...)
        (flat-named-contract (string->symbol name.c) (match-lambda [(list x ...) (and prop ...)])))]))


(module+ test
  (define ctc (list/dc [x number?] [y number?] #:post "a" (x y) (>= x y)))

  (define/contract (f z) (-> ctc number?) 42)

  (check-exn exn:fail:contract? (λ () (f '(0 1))))
  #;(f '(0 1)))
