#lang racket/base

(require
 "../util/measure.rkt"
 (only-in "spreadsheet.rkt" rktd->spreadsheet)
 (only-in "summary.rkt" get-project-name from-rktd)
 (only-in "lnm-plot.rkt" lnm-plot))

;; Just testing

(define l-list '(0 1 2 3))
(define NUM_SAMPLES 100)

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 3416]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main filename)
  ;; Parse data from input file (also creates module graph)
  (define summary (from-rktd filename))
  (define name (get-project-name summary))
  ;; Create L-N/M pictures
  (lnm-plot summary #:L l-list
            #:N 3
            #:M 10
            #:max-overhead 20
            #:cutoff-proportion 0.6
            #:num-samples NUM_SAMPLES
            #:plot-height 300
            #:plot-width 400)
  (rktd->spreadsheet filename #:output "./test-case-output.out" #:format 'tab)
  (done))

(define-main-module
  #:entry-point main
  #:data "base/data/snake.rktd"
  #:convert path->string)
