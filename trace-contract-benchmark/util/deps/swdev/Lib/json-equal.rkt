#lang racket

;; compare JSexpr with "up to precision" for numbers

;; ---------------------------------------------------------------------------------------------------
(provide
 #; { JSexpr JSexpr -> Boolean}
 #; (json-equal? j1 j2 #:inexact-okay? p)
 ;; are j1 and j2 equal? up to a precision of p for two numbers?
 (contract-out
  [json-equal? (->i ([j1 any/c] [j2 any/c])
                    (#:inexact-okay? [ok (or/c #false (and/c real? (</c 1.0) (>/c 0.0)))])
                    (r boolean?))]))

;; ---------------------------------------------------------------------------------------------------
(require json)
(module+ test
  (require (submod ".."))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define (json-equal? x y #:inexact-okay? [p #false])
  (define jsnull (json-null))
  (let loop ([x x] [y y])
    (cond
      [(or (integer? x) (real-real? x))
       (and (or (integer? y) (real-real? y)) (if (not p) (equal? x y) (=~ x y p)))]
      [(boolean? x)   (equal? x y)]
      [(eq? x jsnull) (equal? x y)]
      [(string? x)    (equal? x y)]
      [(list? x)      (and (list? y) (= (length x) (length y)) (andmap loop x y))]
      [(hash? x)      (and (hash? y)
                           (= (length (hash->list x)) (length (hash->list y)))
                           (for/and ([(xkey x) x])
                             (and (hash-has-key? y xkey)
                                  (loop x (hash-ref y xkey)))))]
      [else #false])))

(define (=~ a b c)
  (<= (magnitude (- a b)) c))

#; [Any -> Boolean : a proper real number]
(define (real-real? x) ; not nan or inf
  (and (inexact-real? x) (not (member x '(+nan.0 +inf.0 -inf.0)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-exn exn:fail:contract? (λ () (json-equal? "a" "a" #:inexact-okay? 0.5+i)))
  (check-exn exn:fail:contract? (λ () (json-equal? "a" "a" #:inexact-okay? 0)))
  (check-exn exn:fail:contract? (λ () (json-equal? "a" "a" #:inexact-okay? +nan.0)))

  (check-false (json-equal? 3.0 3))
  (check-true (json-equal? 3.0 3 #:inexact-okay? .1))
  (check-true (json-equal? 3 3.0 #:inexact-okay? .1))
  (check-true (json-equal? 3.1 3.0999999999 #:inexact-okay? .1))

  (check-true (json-equal? #f #f))
  (check-false (json-equal? #f 3))

  (check-true (json-equal? (json-null) (json-null)))

  (check-true (json-equal? "🤪" "🤪"))
  (check-false (json-equal? "🤪" "🤪 hello"))

  (check-false (json-equal? (hash 'a 3.0 'b "hello world") (hash 'b "hello world"  'a 3)))
  (check-true
   (json-equal? (hash 'a 3.0 'b "hello world") (hash 'b "hello world"  'a 3) #:inexact-okay? .1))
  
  (check-false (json-equal? (list "a" 3.0 "b" "hello world") (list "a" 3 "b" "hello world")))
  (check-true
   (json-equal? (list "a" 3.0 "b" "hello world") (list "a" 3 "b" "hello world") #:inexact-okay? .1))

  (check-false (json-equal? cons cons) "the function dispatches on JSON only")
  (check-false (json-equal? 'a 'a) "the function dispatches on JSON only"))