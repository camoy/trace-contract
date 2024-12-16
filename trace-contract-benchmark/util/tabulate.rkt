#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data-frame
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/string
         racket/runtime-path
         sawzall
         scribble-abbrevs
         syntax-sloc
         threading
         math/statistics
         "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define-runtime-path PARENT "..")

(define HEADINGS
  `(["Benchmark"
     "l"
     ,(λ (name info none no-load with-load)
        (format "\\bench{~a}" (string-replace name "-two" "2")))]

    ["Base"
     "r"
     ,(λ (name info none no-load with-load)
        (hash-ref info 'base))]

    ["Contract"
     "r"
     ,(λ (name info none no-load with-load)
        (hash-ref info 'contract))]

    #;["Predicate"
       "r"
       ,(λ (name info none no-load with-load)
          (hash-ref info 'predicate))]

    #;["Difficulty"
       "c"
       ,(λ (name info none no-load with-load)
          (format "\\Stars{~a}" (hash-ref info 'difficulty)))]

    ["Checks"
     "r"
     ,(λ (name info none no-load with-load)
        (add-commas (hash-ref no-load 'collections)))]

    ["\\multicolumn{1}{c|}{Disabled}"
     "d{-1}"
     ,(λ (name info none no-load with-load)
        (mean+stddev (hash-ref none 'times)))]

    ["\\multicolumn{1}{c|}{Enabled}"
     "d{-1}"
     ,(λ (name info none no-load with-load)
        (mean+stddev (hash-ref no-load 'times)))]

    ["\\multicolumn{1}{c|}{Predicate}"
     "d{-1}"
     ,(λ (name info none no-load with-load)
        (mean+stddev (hash-ref with-load 'times)))]

    ["Overhead"
     "r"
     ,(λ (name info none no-load with-load)
        (define old (mean (hash-ref none 'times)))
        (define new (mean (hash-ref no-load 'times)))
        (define pct (round (* 100 (/ (- new old) old))))
        (if (zero? pct)
            (format "\\(\\approx\\) ~a\\%" pct)
            (format "~a\\%" pct)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(define (print-table path)
  (define df (df-read/csv path))
  (define ht (benchmark-hash df))
  (define rows
    (for/list ([row (in-list BENCHMARKS)])
      (for/list ([heading (in-list HEADINGS)])
        (match-define (list name (list none no-load with-load) _) row)
        (match-define (list _ _ proc) heading)
        (~a
         (proc name
               (sloc-hash row)
               (hash-ref ht (list name none))
               (hash-ref ht (list name no-load))
               (hash-ref ht (list name with-load)))))))
  (define rows-strs (map (λ (row) (string-join row " & ")) rows))
  (define str (string-join rows-strs " \\\\\n"))
  (displayln
   (format "\\begin{tabular}{~a}"
           (string-join (map second HEADINGS) " | ")))
  (displayln (string-join (map first HEADINGS) " & \n"))
  (displayln "\\\\ \\hline")
  (displayln str)
  (displayln "\\end{tabular}"))

(define (benchmark-hash df)
  (define vec
    (~> df
        (group-with "benchmark" "level")
        (aggregate [times (cpu) cpu]
                   [collections (collections) (assert-singleton collections)])
        ungroup
        (df-select* "benchmark" "level" "collections" "times")))
  (for/hash ([row (in-vector vec)])
    (match-define (vector benchmark level collections times) row)
    (values (list benchmark level)
            (hash 'collections collections
                  'times (vector->list times)))))

(define (sloc-hash benchmark-info)
  (match-define (list benchmark _ ctc-paths) benchmark-info)
  (define root (build-path PARENT benchmark))
  (hash 'base (directory-sloc root)
        'contract (include-slocs (adjust-paths root ctc-paths))))

(define (adjust-paths dir-path paths)
  (for/list ([p (in-list paths)])
    (normalize-path (build-path dir-path p))))

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

(define (assert-singleton xs)
  (define xs* (remove-duplicates (vector->list xs)))
  (unless (= (length xs*) 1)
    (error 'assert-singleton))
  (first xs*))

(define (mean+stddev x)
  (format "~a . ~a"
          (inexact->exact (round (mean x)))
          (inexact->exact (round (stddev x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (match-define (vector input-path) (current-command-line-arguments))
  (print-table input-path))
