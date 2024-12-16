#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract/private/attribute-contract
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ example
  (provide (all-defined-out))

  (struct in-port (v))
  (define in-port-key (make-attribute 'ipk))
  (define in-port/c (attribute/c in-port? in-port-key #t))
  (define in-port-open? (attribute-satisfies/c in-port-key values))
  (define close-in-port/c (and/c in-port-open? (attribute-set/c in-port-key #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (submod ".." example)
           chk)

  (define/contract (open-input-string* str)
    (-> string? in-port/c)
    (in-port (open-input-string str)))

  (define/contract (bad x)
    (-> any/c in-port/c)
    x)

  (define/contract (read* in)
    (-> in-port-open? any/c)
    (read (in-port-v in)))

  (define/contract (close-input-port* in)
    (-> close-in-port/c void?)
    (close-input-port (in-port-v in)))

  (define/contract (close-indy in x)
    (->i ([p close-in-port/c] [x (p) any/c])
         [res void?])
    (close-input-port (in-port-v in)))

  (chk
   (pairs->hash '(x 1 y 2))
   (hash 'x 1 'y 2)

   (attribute-contract-name in-port/c)
   '(attribute/c in-port? ipk #t)

   #:do (define things (open-input-string* "1 2"))
   #:t (attribute-present? in-port-key things)
   #:x (bad "hello")
   "promised: in-port?"

   (list (read* things) (read* things))  '(1 2)
   #:x (read* 42)
   "42 does not have key ipk"

   #:do (close-input-port* things)
   #:x (close-input-port* things)
   "accumulator #f does not satisfy values"

   ;; Multiple wrappers keeps the old accumulator.
   #:x (close-input-port* (bad things))
   "accumulator #f does not satisfy values"

   ;; Indy doesn't cause multiple effects.
   #:do (define things2 (open-input-string* "3 4"))
   (list (read* things2) (read* things2))  '(3 4)
   #:do (close-indy things2 42)
   ))
