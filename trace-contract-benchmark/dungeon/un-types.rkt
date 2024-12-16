#lang racket/base

(provide
  index?
  assert
)

(define (assert v p)
  (if (p v)
    v
    (raise-user-error 'assert)))

(require (only-in racket/unsafe/ops unsafe-fx>=))

(define (index? x) (and (fixnum? x) (unsafe-fx>= x 0) (fixnum? (* x 4))))
