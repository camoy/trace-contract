#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data-frame
         racket/format
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         sawzall
         whereis
         "constants.rkt"
         "measure.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more constants

(define DATA-FMT
  #px"cpu time: (\\d+) real time: (\\d+) gc time: (\\d+)\n(\\d+)\n(\\d+)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(define (output-stats df benchmark level exe-path output-path)
  (define df* (df-add-rows df (stats benchmark level exe-path)))
  (df-write/csv df* output-path)
  df*)

(define (stats benchmark level exe-path)
  (define data
    (with-handlers ([exn:fail? (λ (e) (eprintf (~v e)))])
      (do-benchmark benchmark level exe-path)))
  (if (not data)
      (list (vector 0 benchmark level #f #f #f #f))
      (for/list ([datum (in-list data)]
                 [sample (in-naturals)])
        (match-define (list cpu real gc projections collections) datum)
        (vector sample benchmark level cpu real gc projections collections))))

(define (do-benchmark benchmark level exe-path)
  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv LVL-VAR level)
    (for/list ([n (in-range ITERS)])
      (match (subprocess/timeout "racket" (path->string exe-path))
        [(pregexp DATA-FMT (list _ cpu real gc projections collections))
         (map string->number (list cpu real gc projections collections))]
        [#f #f]))))

(define (subprocess/timeout command . args)
  (define cust (make-custodian))
  (define-values (subproc stdout stdin stderr)
    (parameterize ([current-subprocess-custodian-mode 'kill]
                   [current-custodian cust])
      (apply subprocess #f #f #f (find-executable-path command) args)))
  (define err-str (port->string stderr))
  (unless (equal? err-str "")
    (eprintf err-str))
  (begin0
    (and (sync/timeout TIMEOUT-SECS subproc) (port->string stdout))
    (custodian-shutdown-all cust)))

(define (df-init)
  (define df (make-data-frame))
  (for ([column (in-list COLUMNS)])
    (df-add-series! df (make-series column #:data (vector))))
  df)

(define (df-add-rows df rows)
  (define new-rows-df (make-data-frame))
  (for ([column (in-list COLUMNS)]
        [k (in-naturals)])
    (define data
      (for/vector ([row (in-list rows)])
        (vector-ref row k)))
    (df-add-series! new-rows-df (make-series column #:data data)))
  (combine df new-rows-df))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `benchmarks-prepare`

(define (benchmarks-prepare modifiers)
  (define-values (prepares cleanups)
    (for/lists (p c)
               ([modifier (in-list modifiers)])
      (apply make-prepare-and-cleanup modifier)))
  (let ([old-exit-handler (exit-handler)])
    (exit-handler
     (λ (status)
       (for-each (λ (cleanup) (cleanup)) cleanups)
       (refresh-bytecode)
       (old-exit-handler status))))
  (for-each (λ (prepare) (prepare)) prepares)
  (refresh-bytecode))

(define (make-prepare-and-cleanup source target support)
  (define tmp-file (make-temporary-file))
  (define mod-abs (whereis-module target))
  (define mod-parent (path-only mod-abs))
  (define target-support
    (and support
         (let-values ([(base final dir?) (split-path (path->complete-path support))])
           (build-path mod-parent final))))
  (values (λ ()
            (copy-file mod-abs tmp-file #t)
            (copy-file source mod-abs #t)
            (when support
              (copy-directory/files support target-support)))
          (λ ()
            (copy-file tmp-file mod-abs #t)
            (when support
              (delete-directory/files target-support))
            (delete-file tmp-file))))

;; This needs to happen in a new process otherwise we might get a duplicate
;; instantiation of `racket/gui`.
(define (refresh-bytecode)
  (subprocess/timeout "racket" "refresh-bytecode.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (match-define (vector output-path) (current-command-line-arguments))
  (benchmarks-prepare MODIFIERS)
  (void
   (for/fold ([df (df-init)])
             ([benchmark-info (in-list BENCHMARKS)]
              [benchmark-path (in-list BENCHMARK-PATHS)])
     (match-define (list benchmark levels _) benchmark-info)
     (for/fold ([df df])
               ([level (in-list levels)])
       (output-stats df benchmark level benchmark-path output-path)))))
