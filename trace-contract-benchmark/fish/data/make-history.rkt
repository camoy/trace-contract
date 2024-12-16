#lang racket/base

(define (make-in r c f d p)
  (make-hasheq `([row . ,r]
                 [column . ,c]
                 [fish . ,f]
                 [players . ,(map (λ (p) (list p d)) p)])))

(define (make-players n)
  (for/list ([k (in-range n)])
    (natural->string k)))

(define (natural->string k)
  (define char-list
    (let go ([k k])
      (define char (natural->char (modulo k 26)))
      (if (< k 26)
          (list char)
          (cons char (go (floor (/ k 26)))))))
  (apply string (reverse char-list)))

(define (natural->char k)
  (integer->char (+ 97 k)))

(define MIN 1)
(define DEPTH 2)

(module+ main
  (require racket/cmdline
           racket/pretty)

  (command-line
   #:args (max-dim-str max-fish-str max-players-str tests-str out-path)
   (define max-dim (string->number max-dim-str))
   (define max-fish (string->number max-fish-str))
   (define max-players (string->number max-players-str))
   (define tests (string->number tests-str))
   (with-output-to-file out-path #:exists 'replace
     (λ ()
       (pretty-write
        (for/list ([_ (in-range tests)])
          (make-in (random MIN max-dim)
                   (random MIN max-dim)
                   (random MIN max-fish)
                   DEPTH
                   (make-players (random MIN max-players)))))))
   (void)))
