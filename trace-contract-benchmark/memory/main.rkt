#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require plot
         racket/contract
         racket/function
         racket/include
         racket/list
         racket/match
         racket/stream
         trace-contract
         "../util/measure.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (cumulative-> rng-ctc)
  (->i ([x (or/c #f 'cumulative custodian?)])
       [result (x) (if (eq? x 'cumulative)
                       rng-ctc
                       exact-nonnegative-integer?)]))

(include "trace-contract.rkti")

(define-modifiable
  #:level base
  (define cmu/c (cumulative-> exact-nonnegative-integer?))
  (define done (λ () (for ([_ 10000]) (log-info "heisenberg"))))

  #:level noop
  (define cmu/c (make-memory-fold/c (λ (acc x) #t)))
  (define done void)

  #:level check
  (define cmu/c (make-memory-fold/c memory-folder))
  (define done void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define Y-MIN-MB 250)
(define Y-MAX-MB 500)
(define MAX-DATA 7)
(define ITERS 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(define/contract current-memory-use-protected cmu/c current-memory-use)

(define (main)
  (for ([_ (in-range ITERS)])
    (make-plot-renderers))
  (done))

(define make-plot-renderers
  (let ([ys null])
    (λ ()
      (define next-y (current-memory-use-protected 'cumulative))
      (set! ys (append-clamp MAX-DATA ys next-y))
      (list
       (lines-interval
        (make-posns (const 0) ys)
        (make-posns byte->megabyte ys))))))

(define (append-clamp k xs x)
  (if (< (length xs) k)
      (append xs (list x))
      (append (rest xs) (list x))))

(define (make-posns f ys)
  (for/list ([x (in-naturals)]
             [y (in-list ys)])
    (vector x (f y))))

(define (byte->megabyte x)
  (/ x 10e6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define-main-module
  #:entry-point main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui

#;(module+ gui
    (require racket/class
             racket/gui
             plot-container)

    (define toplevel
      (new frame%
           [label "Cumulative Memory"]
           [height 550]
           [width 400]))
    (define container (new plot-container% [parent toplevel]))
    (define snip
      (plot-snip (make-plot-renderers)
                 #:title "Cumulative Memory"
                 #:x-min 0 #:x-max (- MAX-DATA 1)
                 #:y-min Y-MIN-MB #:y-max Y-MAX-MB
                 #:x-label "Time"
                 #:y-label "Memory (mB)"))
    (send container set-snip snip)
    (send toplevel show #t)

    (define (do-animation fn #:frame-rate [frame-rate 60])
      (define frame-time (/ 1 frame-rate))
      (define start-timestamp (current-inexact-milliseconds))
      (let loop ([timestamp (current-inexact-milliseconds)])
        (send snip set-overlay-renderers (fn))
        (collect-garbage 'incremental)
        (define Δ-time (- (current-inexact-milliseconds) timestamp))
        (define remaining (- frame-time (/ Δ-time 1000)))
        (sleep/yield (max 0 remaining))
        (loop (current-inexact-milliseconds))))

    (do-animation make-plot-renderers #:frame-rate 1))
