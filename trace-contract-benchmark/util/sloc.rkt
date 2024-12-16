#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         racket/path
         racket/runtime-path
         syntax-sloc
         "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define-runtime-path PARENT "..")

(define (adjust-paths dir-path paths)
  (for/list ([p (in-list paths)])
    (normalize-path (build-path dir-path p))))

(define ((not-member-path? path-list) x)
  (and (not (member (normalize-path x) path-list))
       (equal? (filename-extension x) #"rkt")))

(define (include-slocs paths)
  (for/sum ([p (in-list paths)])
    (define fp (open-input-file p))
    (port-count-lines! fp)
    (define data
      (let go ()
        (define datum (read-syntax '_ fp))
        (if (eof-object? datum) null (cons datum (go)))))
    (begin0
      (syntax-sloc (datum->syntax #f data))
      (close-input-port fp))))

(module+ main
  (void
   (for ([benchmark-info (in-list BENCHMARKS)])
     (match-define (list benchmark _ ctc-paths) benchmark-info)
     (define dir-path (build-path PARENT benchmark))
     (define ctc-paths* (adjust-paths dir-path ctc-paths))
     (define base-sloc (directory-sloc dir-path #:use-file? (not-member-path? ctc-paths*)))
     (define ctc-sloc (include-slocs ctc-paths*))
     (displayln (list benchmark base-sloc ctc-sloc)))))
