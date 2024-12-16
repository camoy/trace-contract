#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-modifiable
         define-main-module
         id
         always-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/set
                     syntax/parse)
         racket/contract
         racket/file
         racket/logging
         "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `define-modifiable`

(begin-for-syntax
  (define-syntax-class def
    #:literals (define define-values)
    (pattern (define (y:id a ...) es:expr ...)
             #:with (x ...) #'(y)
             #:with norm #'(set! y (λ (a ...) es ...)))
    (pattern (define y:id e:expr)
             #:with (x ...) #'(y)
             #:with norm #'(set! y e))
    (pattern (define-values (x:id ...) e:expr)
             #:with norm #'(set!-values (x ...) e)))

  (define (extract-defs ds)
    (define ds* (map list->set (syntax->datum ds)))
    (define d-fst (car ds*))
    (define all-equal
      (for/and ([d (in-list (cdr ds*))])
        (equal? d-fst d)))
    (unless all-equal
      (error 'extract-defs "levels define different identifiers"))
    (car (syntax-e ds))))

(define-syntax (define-modifiable stx)
  (syntax-parse stx
    #:literals (define)
    [(_ (~seq #:level ?lvl:id ?d:def ...) ...)
     #:with (?lvl-str ...) (map symbol->string (syntax->datum #'(?lvl ...)))
     #:with (?y ...) (extract-defs #'([?d.x ... ...] ...))
     #'(begin
         (define ?y #f) ...
         (case (getenv LVL-VAR)
           [(?lvl-str) ?d.norm ...] ...
           [else (error 'define-modifiable "level undefined")]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging


(define (id x) x)
(define (always-true x) #t)

(define (intercepted-logs thk)
  (define projection 0)
  (define check 0)
  (with-intercepted-logging
    (λ (vec)
      (define msg (vector-ref vec 1))
      (cond
        [(member msg '("trace-contract: projection"
                       "trace-contract: projection"))
         (set! projection (add1 projection))]
        [(member msg '("trace-contract: check"
                       "trace-contract: check"))
         (set! check (add1 check))]))
    thk
    #:logger (current-logger)
    'info)
  (displayln projection)
  (displayln check))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `define-main-module`

(define-syntax (define-main-module stx)
  (syntax-parse stx
    [(_ #:entry-point ?main:id
        (~optional (~seq #:data ?data:str))
        (~optional (~seq #:convert ?data-convert)
                   #:defaults ([?data-convert #'file->value])))
     #:with ?def-cwd-rtp
     (datum->syntax stx (syntax-e #'(define-runtime-path CWD ".")) stx)
     #'(begin
         (module+ main
           (require racket/runtime-path
                    racket/file)
           ?def-cwd-rtp
           (define args (~? (list (?data-convert (build-path CWD ?data))) null))
           (collect-garbage 'major)
           (intercepted-logs
            (λ ()
              (time (apply ?main args))))))]))
