#lang racket

;; some simple facilities for writing tests to two files:
;; <n>-in.json for the test input
;; <n>-out.json for the test output

;; NOTE this file is in dire need of some abstraction work 

(provide
 #; {(U Path-String #false) -> Void}
 ;; path-string -> write the test files into this directory; #false means no output 
 recording

 #; {Natural -> Void}
 ;; start numbering the test files from here: 0, 1, 2, ... is default
 #; (start-at 0) ; gets us 1, 2, 3 ...
 start-at 

 ;; SYNTAXES:

 ;; like r-check-equal? but with an equality predicate passed in first 
 r-check
 
 #; {[-> Void] [Listof JSexpr] [Listof JSexpr] String -> Void}
 ;; convert inputs and expected to JSON, run main on converted inputs, compare with expected
 ;; IF recording is set, also record the specified test cases as pairs of files
 r-check-equal?

 #; {[-> Void] [Listof JSexpr] [Listof JSexpr] String -> Void}
 ;; convert inputs + expected to JSON, run main on converted ins, compare via diff expect #false
 ;; IF recording is set, also record the specified test cases as pairs of files
 r-check-diff

 #;(r-check-exn Expected main Inputs Message)
 ;; convert inputs to JSON, run main on converted inputs, use check-exn with Expected,
 ;; NO RECORDING 
 r-check-exn)

;; ---------------------------------------------------------------------------------------------------
(require "communication.rkt")
(require "../Debugging/diff.rkt")
(require "../Lib/should-be-racket.rkt")
(require rackunit)
(require json)

;; ---------------------------------------------------------------------------------------------------
(define recording (make-parameter #false))
(define start-at  (make-parameter -1))

(define-syntax (r-check-equal? stx)
  (syntax-case stx ()
    [(r-check-equal? main inputs expected msg)
     #`(begin
         (define in:str (prepare inputs))
         (define ex:str (prepare expected))

         (define actual (gensym))
         (define (tee x) (set! actual x) x)

         (void msg)

         #,(syntax/loc stx
             (check-equal?
              (tee (post (with-output-to-bytes (lambda () (with-input-from-bytes in:str main)))))
              expected
              msg))
  
         (record inputs actual))]))

(define-syntax (r-check stx)
  [syntax-case stx ()
    [(r-check -equal? main inputs expected msg)
     #`(begin
         (define in:str (prepare inputs))
         (define ex:str (prepare expected))

         (define actual (gensym))
         (define (tee x) (set! actual x) x)

         #,(syntax/loc stx
             (check
              -equal?
              (tee (post (with-output-to-bytes (lambda () (with-input-from-bytes in:str main)))))
              expected
              msg))
  
         (record inputs actual))]])

(define-syntax (r-check-diff stx)
  (syntax-case stx ()
    [(r-check-diff main inputs expected msg)
     #`(begin 
         (define in:str (prepare inputs))
         (define ex:str (prepare expected))

         (define actual (gensym))
         (define (tee x) (set! actual x) x)

         #,(syntax/loc stx
             (check-false
              (diff
               (tee (post (with-output-to-bytes (lambda () (with-input-from-bytes in:str main)))))
               expected)
              msg))
  
         (record inputs actual))]))

(define-syntax (r-check-exn stx)
  (syntax-case stx ()
    [(r-check-exn expected main inputs msg)
     #`(begin
         (define in:str (prepare inputs))
         
         (define actual (gensym))
         (define (tee x) (set! actual x) x)

         #,(syntax/loc stx 
             (check-exn
              expected
              (λ ()
                (dev/null
                 (with-output-to-bytes (λ () (with-input-from-bytes in:str main)))) msg))))]))

#;[[Listof Jsexpr] [Listof Jsexpr] -> Void]
;; write test input and test output to next pair of test files in (recording) directory, if any 
(define (record input output #:write-inputs (wi send-message))
  (unless (symbol? output)
    (define base (recording))
    (when base
      (unless (directory-exists? base) (make-directory base))
      (start-at (+ (start-at) 1))
      (define n (~a (start-at)))
      (define -in.json  (build-path base (format "~a-in.json" n)))
      (define -out.json (build-path base (format "~a-out.json" n)))
      (write-to -in.json input wi)
      (write-to -out.json output (lambda (x) (send-message x))))))

;; [X] [PathString [Listof X] [X -> Void] -> Void]
;; write and optionally replace file 
(define (write-to file-name input writer)
  (with-output-to-file file-name (lambda () (for ((x input)) (writer x))) #:exists 'replace))

#; {(U String [Listof JSexpr]) -> Bytes}
(define (prepare x)
  (cond
    [(string? x) (string->bytes/utf-8 x)]
    [else (bytes-append (bytes-join (map jsexpr->bytes x) #" "))]))

#;{Bytes -> [Listof JSexpr]}
(define (post x)
  (port->list read-json (open-input-bytes x)))
  
