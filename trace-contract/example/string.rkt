#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "increment.rkt"
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (sequence-ok? y)
  (for/and ([y* (in-list (stream-group-by y first))])
    (increments? (map second y*))))

;; For a function that will use the given string to determine the state.
(define counter/c
  (trace/c ([y integer?])
    (->i ([x string?])
         [result (x) (list/t x y)])
    (full (y) sequence-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   ;; Counter that correctly uses the argument to distinguish different states.
   #:do
   (define/contract counter
     counter/c
     (let ([state (make-hash)])
       (λ (s)
         (if (hash-has-key? state s)
             (hash-update! state s add1)
             (hash-set! state s 1))
         (hash-ref state s))))

   (list (counter "a") (counter "b")
         (counter "a") (counter "b")
         (counter "a") (counter "b")
         (counter "c") (counter "c"))
   '(1 1 2 2 3 3 1 2)

   ;; Counter that will not work correctly for the argument "c" string. It will
   ;; always return 42.
   #:do
   (define/contract wrong-counter
     counter/c
     (let ([state (make-hash)])
       (λ (s)
         (if (hash-has-key? state s)
             (hash-update! state s add1)
             (hash-set! state s 1))
         (if (equal? s "c")
             42
             (hash-ref state s)))))

   (list (wrong-counter "a") (wrong-counter "a"))
   '(1 2)

   #:x
   (wrong-counter "c")
   (trace-exn? '(definition wrong-counter)
               (current-contract-region)
               "y: ((a 1) (a 2) (c 42))")
   ))
