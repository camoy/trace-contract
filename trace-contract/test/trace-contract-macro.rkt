#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           syntax/macro-testing)

  (chk
   #:x
   (convert-syntax-error
    (trace/c ([x integer?] [x integer?])
      any/c
      (accumulate 0 [(x) values])))
   "duplicate identifier"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(x x) values])))
   "duplicate identifier"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(y) values])))
   "trace variable not declared"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c))
   "expected more terms starting with trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate)))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0)))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [() values])))
   "invalid trace clause"

   #:x
   (convert-syntax-error
    (accumulate 0))
   "trace clause must occur within trace/c"

   #:x
   (contract
    (trace/c ([x integer?])
      #'hello
      (accumulate 0 [(x) values]))
    0 'pos 'neg)
   "macro argument contract on inner contract"

   #:x
   (contract
    (trace/c ([x #'hello])
      any/c
      (accumulate 0 [(x) values]))
    0 'pos 'neg)
   "macro argument contract on variable contract"

   #:x
   (contract
    (trace/c ([x integer?])
      any/c
      (accumulate 0 [(x) (λ () 42)]))
    0 'pos 'neg)
   "contract violation"
   ))