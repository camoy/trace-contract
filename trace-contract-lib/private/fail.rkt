#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out fail)
         make-fail
         NO-RESET)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `fail` value, as an accumulator, signals a contract error.
;;   - `reset` : Any
;;   - `explain` : (-> Any)
(struct fail (reset explain))

;; Symbol that indicates absence of a reset.
(define NO-RESET (gensym 'NO-RESET))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructor

(define (make-fail #:reset [reset NO-RESET] #:explain [explain #f])
  (fail reset explain))
