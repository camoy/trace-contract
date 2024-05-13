#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require automata/machine
         automata/re
         automata/re-ext
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Recognizes iterators (which are boxes in this case).
(define iterator? box?)

;; Given a list, returns an iterator for that list.
(define iterator box)

;; Returns if the iterator has another element.
(define (-has-next? iter) (not (empty? (unbox iter))))

;; Returns the next element in the iterator.
(define (-next! iter)
  (begin0
    (car (unbox iter))
    (set-box! iter (cdr (unbox iter)))))

;; A regular expression for recognizing that a call to next must be preceded by
;; a call to has-next.
(define HAS-NEXT-RE
  (re (seq (star (seq (plus 'h) 'n))
           (star 'h))))

(define (sequence-ok? t)
  (for/and ([xs (stream-group-by t first eq?)])
    (machine-accepts? HAS-NEXT-RE (stream-map second xs))))

;; For enforcing that has-next is called before every next.
(define (make-contracts)
  (trace/c ([t iterator?])
    #:global
    (values
     ;; has-next?
     (-> (list/t t 'h) boolean?)
     ;; next!
     (-> (list/t t 'n) any))
    (full (t) sequence-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (define iter (iterator '(a b c)))
   #:do
   (define-values/contract (has-next? next!)
     (make-contracts)
     (values -has-next? -next!))
   #:t (has-next? iter)
   (next! iter) 'a
   #:t (has-next? iter)
   (next! iter) 'b
   #:t (has-next? iter)
   (next! iter) 'c
   #:! #:t (has-next? iter)

   #:do (define iter2 (iterator '(a b)))
   #:do
   (define-values/contract (has-next2? next2!)
     (make-contracts)
     (values -has-next? -next!))
   #:t (has-next2? iter2)
   (next2! iter2) 'a
   #:x
   (next2! iter2)
   (trace-exn? (current-contract-region)
               '(definition next2!)
               "t: ((#&(b) h) (#&(b) n) (#&(b) n))")
   ))
