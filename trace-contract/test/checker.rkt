#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (submod trace-contract/private/checker private)
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; TODO
#;(module+ test
  (require chk
           racket/contract/private/blame
           racket/stream
           (submod "clause.rkt" example))

  (define (make-blame* pos)
    (make-blame (srcloc #f #f #f #f #f) #f (λ () #f) pos #f #t))
  (define blm (make-blame* 'pos))

  (chk
   #:do (match-define (hash-table ('x (stream pos-ch)))
          (make-checker-hash (list positive-clause)))
   #:do (match-define (hash-table ('x (stream even-ch)) ('y (stream ven-ch)))
          (make-checker-hash (list even-clause)))

   #:do (checker-update! pos-ch 'x blm 10 'neg)
   #:x (checker-update! pos-ch 'x blm -20 'neg)
   "accumulator: 11"

   #:do (checker-update! even-ch 'x blm 10 'neg)
   #:do (checker-update! even-ch 'y blm 6 'neg)
   #:do (checker-update! even-ch 'x blm 10 'neg)
   #:x (checker-update! even-ch 'y (blame-swap blm) 3 'neg)
   "accumulator: 16"
   ))
