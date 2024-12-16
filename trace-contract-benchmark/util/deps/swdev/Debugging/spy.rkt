#lang racket

(provide
 #; { (spy e) || (spy e #:with show) }
 #; {show : (-> Any Syntax Nat Nat Void)}
 ;; evaluates e to v
 ;; EFFECT render v via printing it to the error port or with `show`
 spy)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (spy exp)
  (syntax-parse exp
    [(spy e (~optional [~seq #:with rendering] #:defaults ([rendering #'to-error-port])))
     (with-syntax ([expr #'e]
                   [src  (syntax-source (syntax e))]
                   [line (syntax-line #'e)]
                   [col  (syntax-column #'e)])
       #'(with-handlers ([exn:fail? (λ (xn)
                                      (define xn-msg (~a "Exception \n" xn "\n"))
                                      (rendering xn-msg 'expr src `(line: ,line col: ,col))
                                      (raise xn))])
           (call-with-values (λ () e)
                             (λ val*
                               (define val (if (null? (rest val*)) (first val*) (cons 'values val*)))
                               (rendering val 'expr src `(line: ,line col: ,col))
                               (if (null? (rest val*)) (first val*) (apply values val*))))))]))

(define (to-error-port val expr src loc)
  (fprintf (current-error-port) "Expression~n~a~nat ~a : ~a~nevaluated to~n~a~n~n" expr src loc val))

;; ---------------------------------------------------------------------------------------------------
(module+ test 
  (spy (+ 1 1))
  (spy (+ 1 1) #:with (λ (val expr src loc) (displayln `[val ,val])))

  (with-handlers ([exn:fail? void])
    (spy (first '[]))
    "expected exn, got result"))