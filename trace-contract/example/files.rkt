#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require automata/machine
         automata/re
         automata/re-ext
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Regular expression that recognizes the proper sequences of operations for
;; file operations. This prefix-closed.
(define FILES-RE
  (re (seq/close 'open
                 (star (union 'read 'write))
                 'close)))

(define (files-re-next acc t)
  (match-define (list handle val) t)
  (define st (hash-ref acc handle (λ () FILES-RE)))
  (define st* (st val))
  (if (machine-accepting? st*)
      (hash-set acc handle st*)
      (fail)))

;; File IO based on functions and strings.
(define (make-contracts)
  (trace/c ([t string?])
    #:global
    (values
     ;; open
     (-> string? (and/c string? (list/t t 'open)))
     ;; read
     (-> (and/c string? (list/t t 'read)) any)
     ;; write
     (-> (and/c string? (list/t t 'write)) string? any)
     ;; close
     (-> (and/c string? (list/t t 'close)) any))
    (accumulate (hash)
     [(t) files-re-next])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/strip-context)
           chk
           racket/syntax)

  (define-syntax (define-file-ops stx)
    (replace-context
     stx
     #'(define-values/contract (open read write close)
         (make-contracts)
         (values
          (λ (str) str)
          (λ (handle) 42)
          (λ (handle str) (void))
          (λ (handle) (void))))))

  (chk
   #:do (define-file-ops)
   #:t
   (begin
     (define h (open "hello"))
     (read h)
     (write h "foo")
     (close h))
   #:t
   (begin
     (define h (open "world"))
     (close h))
   #:x
   (begin
     (define-file-ops)
     (define h (open "h"))
     (close h)
     (read h))
   (trace-exn? (current-contract-region)
               '(definition read)
               "given: '(\"h\" read)")

   #:x
   (begin
     (define-file-ops)
     (define g (open "g"))
     (close g)
     (close g))
   (trace-exn? (current-contract-region)
               '(definition close)
               "given: '(\"g\" close)")

   #:t (machine-accepts? FILES-RE '(open))
   #:t (machine-accepts? FILES-RE '(open close))
   #:t (machine-accepts? FILES-RE '(open read write))
   #:t (machine-accepts? FILES-RE '(open write read write close))
   #:! #:t (machine-accepts? FILES-RE '(close))
   #:! #:t (machine-accepts? FILES-RE '(open close close))
   #:! #:t (machine-accepts? FILES-RE '(open close read))
   ))
