#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [red-black-tree
   (->* ()
        (#:order order?
         #:key-contract contract?
         #:value-contract contract?)
        #:rest (and/c list? (property/c length even?))
        RBT?)]
  [rename RBT? red-black-tree? predicate/c]
  [red-black-tree-ref
   (->i ([self RBT?]
         [key (self) (RBT-key-ctc self)])
        ([_ (-> any)])
        [_ (self) (RBT-val-ctc self)])]
  [red-black-tree-set
   (->i ([self RBT?]
         [key (self) (RBT-key-ctc self)]
         [val (self) (RBT-val-ctc self)])
        ()
        [_ RBT?])]
  [red-black-tree-remove
   (->i ([self RBT?]
         [key (self) (RBT-key-ctc self)])
        ()
        [_ RBT?])]
  [red-black-tree-iterate-least
   red-black-tree-iterate-extreme/c]
  [red-black-tree-iterate-greatest
   red-black-tree-iterate-extreme/c]
  [red-black-tree-iterate-least/>?
   red-black-tree-iterate-search/c]
  [red-black-tree-iterate-least/>=?
   red-black-tree-iterate-search/c]
  [red-black-tree-iterate-greatest/<?
   red-black-tree-iterate-search/c]
  [red-black-tree-iterate-greatest/<=?
   red-black-tree-iterate-search/c]
  [red-black-tree-iterate-first
   red-black-tree-iterate-extreme/c]
  [red-black-tree-iterate-next
   (-> RBT? red-black-tree-iter? red-black-tree-iter?)]
  [red-black-tree-iterate-key
   (-> RBT? red-black-tree-iter? any)]
  [red-black-tree-iterate-value
   (-> RBT? red-black-tree-iter? any)]
  [red-black-tree-iter?
   predicate/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/dict
         racket/bool
         racket/match
         racket/function
         racket/stxparam
         racket/sequence
         racket/stream
         data/order
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     racket/stxparam))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (red-black-tree #:order [ord datum-order]
                        #:key-contract [key-ctc any/c]
                        #:value-contract [val-ctc any/c]
                        . kvs)
  (for/fold ([t (RBT ord (B) key-ctc val-ctc)])
            ([(k v) (in-chunks (in-list kvs) 2)])
    (red-black-tree-set t k v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (red-black-tree-ref self key
                            [failure-result (default-failure-result key)])
  (define <=> (RBT-ord self))
  (let go ([t (RBT-tree self)])
    (match t
      [(B) (failure-result)]
      [(N c a k v b)
       (case (<=> key k)
         [(<) (go a)]
         [(=) v]
         [(>) (go b)])])))

(define ((default-failure-result key))
  (error 'red-black-ref "no value found for key ~a" key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (red-black-tree-set self key value)
  (define <=> (RBT-ord self))
  (define go
    (match-lambda
      [(B) (R (B) key value (B))]
      [(N c a k v b)
       (case (<=> key k)
         [(<) (balance (N c (go a) k v b))]
         [(=) (N c a key value b)]
         [(>) (balance (N c a k v (go b)))])]))
  (struct-copy RBT self [tree (blacken (go (RBT-tree self)))]))

(define balance
  (match-lambda
    [(or (B (R (R a k₁ v₁ b) k₂ v₂ c) k₃ v₃ d)
         (B (R a k₁ v₁ (R b k₂ v₂ c)) k₃ v₃ d)
         (B a k₁ v₁ (R (R b k₂ v₂ c) k₃ v₃ d))
         (B a k₁ v₁ (R b k₂ v₂ (R c k₃ v₃ d))))
     (R (B a k₁ v₁ b) k₂ v₂ (B c k₃ v₃ d))]
    [(or (BB (R a k₁ v₁ (R b k₂ v₂ c)) k₃ v₃ d)
         (BB a k₁ v₁ (R (R  b k₂ v₂ c) k₃ v₃ d)))
     (B (B a k₁ v₁ b) k₂ v₂ (B c k₃ v₃ d))]
    [t t]))

(define blacken
  (match-lambda
    [(R a k v b) (B a k v b)]
    [t t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (red-black-tree-remove-min self)
  (define-values (k v t)
    (remove-min (RBT-tree self)))
  (values k v (struct-copy RBT self [tree t])))

(define remove-min
  (match-lambda
    [(R (B) k v (B)) (values k v (B))]
    [(B (B) k v (B)) (values k v (BB))]
    [(B (B) k₁ v₁ (R a k₂ v₂ b)) (values k₁ v₁ (B a k₂ v₂ b))]
    [(N c a k v b) (let-values ([(k* v* a*) (remove-min a)])
                     (values k* v* (rotate (N c a* k v b))))]))

(define rotate
  (match-lambda
    [(R (BB? axb) k₂ v₂ (B c k₃ v₃ d))
     (balance (B (R (-B axb) k₂ v₂ c) k₃ v₃ d))]
    [(R (B a k₁ v₁ b) k₂ v₂ (BB? czd))
     (balance (B a k₁ v₁ (R b k₂ v₂ (-B czd))))]
    [(B (BB? axb) k₂ v₂ (B c k₃ v₃ d))
     (balance (BB (R (-B axb) k₂ v₂ c) k₃ v₃ d))]
    [(B (B a k₁ v₁ b) k₂ v₂ (BB? czd))
     (balance (BB a k₁ v₁ (R b k₂ v₂ (-B czd))))]
    [(B (BB? awb) k₁ v₁ (R (B c k₂ v₂ d) k₃ v₃ e))
     (B (balance (B (R (-B awb) k₁ v₁ c) k₂ v₂ d)) k₃ v₃ e)]
    [(B (R a k₄ v₄ (B b k₁ v₁ c)) k₂ v₂ (BB? dze))
     (B a k₄ v₄ (balance (B b k₁ v₁ (R c k₂ v₂ (-B dze)))))]
    [t t]))

(define -B
  (match-lambda
    [(BB) (B)]
    [(BB a k v b) (B a k v b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (red-black-tree-remove self key)
  (define <=> (RBT-ord self))
  (define go
    (match-lambda
      [(B) (B)]
      [(R (B) (== key) _ (B)) (B)]
      [(B (R a k v b) (== key) _ (B))
       (B a k v b)]
      [(B (B) (== key) _ (B))
       (BB)]
      [(N c a k v b)
       (case (<=> key k)
         [(<) (rotate (N c (go a) k v b))]
         [(=) (let-values ([(k* v* b*) (remove-min b)])
                (rotate (N c a k* v* b*)))]
         [(>) (rotate (N c a k v (go b)))])]))
  (struct-copy RBT self [tree (go (redden (RBT-tree self)))]))

(define redden
  (match-lambda
    [(B (B? a) k v (B? b))
     (R a k v b)]
    [t t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct red-black-tree-iter (key value))

(define (red-black-tree-iterate-least self)
  (least (RBT-tree self)))

(define least
  (match-lambda
    [(B) #f]
    [(N _ (B) k v _) (red-black-tree-iter k v)]
    [(N _ a k v _) (least a)]))

(define (red-black-tree-iterate-greatest self)
  (greatest (RBT-tree self)))

(define greatest
  (match-lambda
    [(B) #f]
    [(N _ _ k v (B)) (red-black-tree-iter k v)]
    [(N _ _ k v b) (greatest b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-iterate ?name [?recur ?a ?k ?v ?b] ?< ?= ?>)
  (define (?name self key)
    (define <=> (RBT-ord self))
    (define ?recur
      (match-lambda
        [(B) #f]
        [(N _ ?a ?k ?v ?b)
         (case (<=> key ?k)
           [(<) ?<]
           [(=) ?=]
           [(>) ?>])]))
    (?recur (RBT-tree self))))

(define-iterate red-black-tree-iterate-least/>?
  [recur a k v b]
  (or (recur a) (red-black-tree-iter k v))
  (least b)
  (recur b))

(define-iterate red-black-tree-iterate-least/>=?
  [recur a k v b]
  (or (recur a) (red-black-tree-iter k v))
  (red-black-tree-iter k v)
  (recur b))

(define-iterate red-black-tree-iterate-greatest/<?
  [recur a k v b]
  (recur a)
  (greatest a)
  (or (recur b) (red-black-tree-iter k v)))

(define-iterate red-black-tree-iterate-greatest/<=?
  [recur a k v b]
  (recur a)
  (red-black-tree-iter k v)
  (or (recur b) (red-black-tree-iter k v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define red-black-tree-iterate-first
  red-black-tree-iterate-least)

(define (red-black-tree-iterate-next self pos)
  (red-black-tree-iterate-least/>? self (red-black-tree-iter-key pos)))

(define (red-black-tree-iterate-key self pos)
  (red-black-tree-iter-key pos))

(define (red-black-tree-iterate-value self pos)
  (red-black-tree-iter-value pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct RBT (ord tree key-ctc val-ctc)
  #:methods gen:dict
  [(define dict-ref red-black-tree-ref)
   (define dict-set red-black-tree-set)
   (define dict-remove red-black-tree-remove)
   (define dict-iterate-first red-black-tree-iterate-first)
   (define dict-iterate-next red-black-tree-iterate-next)
   (define dict-iterate-key red-black-tree-iterate-key)
   (define dict-iterate-value red-black-tree-iterate-value)]

  #:methods gen:ordered-dict
  [(define dict-iterate-least red-black-tree-iterate-least)
   (define dict-iterate-greatest red-black-tree-iterate-greatest)
   (define dict-iterate-least/>? red-black-tree-iterate-least/>?)
   (define dict-iterate-least/>=? red-black-tree-iterate-least/>=?)
   (define dict-iterate-greatest/<? red-black-tree-iterate-greatest/<?)
   (define dict-iterate-greatest/<=? red-black-tree-iterate-greatest/<=?)]

  #:transparent)

(struct N (color left-child key value right-child) #:transparent)
(struct L-B ())
(struct L-BB ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define red-black-tree-iterate-extreme/c
  (-> RBT? (or/c red-black-tree-iter? #f)))

(define red-black-tree-iterate-search/c
  (->i ([self RBT?]
        [key (self) (RBT-key-ctc self)])
        ()
        [_ (or/c red-black-tree-iter? #f)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(begin-for-syntax
  (define (predicate-id x)
    (format-id x "~a?" x)))

(define-syntax-parser define-color
  [(_ ?name ?color (~optional ?leaf))
   #:with ?pred (predicate-id #'?name)
   #:with ?leaf-pred (if (attribute ?leaf)
                         (predicate-id #'?leaf)
                         #'(const #f))
   #'(begin
       (begin-for-syntax
         (define syntax-color
           (syntax-rules ()
             [(_ l k v r) (N ?color l k v r)]
             (~? [(_) (?leaf)] (~@)))))
       (define (color-pred x)
         (or (?leaf-pred x)
             (and (N? x) (eq? (N-color x) ?color))))
       (define-match-expander ?name syntax-color syntax-color)
       (define-match-expander ?pred
         (syntax-rules ()
           [(_ x) (? color-pred x)])
         (syntax-rules ()
           [(_ x) (color-pred x)])))])

(define-color R 'red)
(define-color B 'black L-B)
(define-color BB 'double-black L-BB)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chunks

(define (in-chunks seq n)
  (define (pos→element pos)
    (apply values (stream->list (stream-take pos n))))

  (define (next-pos pos)
    (stream-drop pos n))

  (define (continue-with-pos? pos)
    (not (stream-empty? pos)))

  (make-do-sequence
   (λ ()
     (values
      pos→element
      next-pos
      (sequence->stream seq)
      continue-with-pos?
      #f
      #f))))

(define (stream-drop s n)
  (let go ([s s]
           [k n])
    (if (zero? k)
        s
        (go (stream-rest s) (sub1 k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           rackunit
           racket/random)

  (test-case "pattern macros"
    (chk
     #:t (R? (R (B) 0 0 (B)))
     #:! #:t (B? (R (B) 0 0 (B)))
     #:! #:t (R? (B))
     #:t (B? (B))
     #:t (B? (B (B) 0 0 (B)))

     (match-let ([(R _ k v _) (R (B) 1 2 (B))])
       (cons k v))
     (cons 1 2)

     #:t (match (B (B) 0 0 (B))
           [(R _ k v _) #f]
           [_ #t])
     #:t (B? (match (B)
               [(B? x) x]
               [_ #f]))
     ))

  (define t₁ (red-black-tree))
  (define t₂ (red-black-tree 'a 1))
  (define t₃ (red-black-tree 'b 2 'c 3))
  (define t₄ (red-black-tree-set t₃ 'a 1))

  (test-case "ref"
    (chk
     #:t (red-black-tree-ref t₁ 'a (λ () #t))
     (red-black-tree-ref t₂ 'a) 1
     (red-black-tree-ref t₃ 'b) 2
     (red-black-tree-ref t₃ 'c) 3
     (red-black-tree-ref t₄ 'a) 1
     (red-black-tree-ref t₄ 'b) 2
     (red-black-tree-ref t₄ 'c) 3
     ))

  (define-values (t₂a t₂1 t₂*) (red-black-tree-remove-min t₂))
  (define-values (t₃b t₃2 t₃*) (red-black-tree-remove-min t₃))
  (define-values (t₃c t₃3 t₃**) (red-black-tree-remove-min t₃*))
  (define-values (t₄a t₄1 t₄*) (red-black-tree-remove-min t₄))
  (define-values (t₄b t₄2 t₄**) (red-black-tree-remove-min t₄*))
  (test-case "remove-min"
    (chk
     t₂a 'a
     t₂1 1
     t₃b 'b
     t₃2 2
     t₃c 'c
     t₃3 3
     t₄a 'a
     t₄1 1
     t₄b 'b
     t₄2 2

     #:t (red-black-tree-ref t₃* 'c (λ () #t))
     (red-black-tree-ref t₃* 'c) 3
     #:t (red-black-tree-ref t₄* 'a (λ () #t))
     (red-black-tree-ref t₄* 'b) 2
     (red-black-tree-ref t₄* 'c) 3
     #:t (red-black-tree-ref t₄** 'b (λ () #t))
     (red-black-tree-ref t₄** 'c) 3))

  (define t₄-b (red-black-tree-remove t₄ 'b))
  (define t₄-a-b (red-black-tree-remove t₄-b 'a))
  (define t₄-a-b-c (red-black-tree-remove t₄-a-b 'c))
  (test-case "remove"
    (chk
     (red-black-tree-ref t₄-b 'a) 1
     #:t (red-black-tree-ref t₄-b 'b (λ () #t))
     (red-black-tree-ref t₄-b 'c) 3

     #:t (red-black-tree-ref t₄-a-b 'a (λ () #t))
     (red-black-tree-ref t₄-a-b 'c) 3

     #:t (red-black-tree-ref t₄-a-b-c 'c (λ () #t))))

  (define val-of red-black-tree-iter-value)
  (test-case "ordered strict iterate"
    (chk
     (val-of (red-black-tree-iterate-least t₂)) 1
     (val-of (red-black-tree-iterate-least t₃)) 2
     (val-of (red-black-tree-iterate-least t₄)) 1

     (val-of (red-black-tree-iterate-greatest t₂)) 1
     (val-of (red-black-tree-iterate-greatest t₃)) 3
     (val-of (red-black-tree-iterate-greatest t₄)) 3

     #:! #:t (red-black-tree-iterate-least/>? t₂ 'a)
     (val-of (red-black-tree-iterate-least/>? t₃ 'b)) 3
     (val-of (red-black-tree-iterate-least/>? t₄ 'a)) 2
     (val-of (red-black-tree-iterate-least/>? t₄ 'b)) 3
     #:! #:t (red-black-tree-iterate-least/>? t₄ 'c)

     #:! #:t (red-black-tree-iterate-greatest/<? t₂ 'a)
     (val-of (red-black-tree-iterate-greatest/<? t₃ 'c)) 2
     (val-of (red-black-tree-iterate-greatest/<? t₄ 'c)) 2
     (val-of (red-black-tree-iterate-greatest/<? t₄ 'b)) 1
     #:! #:t (red-black-tree-iterate-greatest/<? t₄ 'a)
     ))

  (define t₅ (red-black-tree 1 1 5 5 10 10))
  (test-case "ordered non-strict iterate"
    (chk
     (val-of (red-black-tree-iterate-least/>=? t₅ 0)) 1
     (val-of (red-black-tree-iterate-least/>=? t₅ 1)) 1
     (val-of (red-black-tree-iterate-least/>=? t₅ 2)) 5
     (val-of (red-black-tree-iterate-least/>=? t₅ 5)) 5
     (val-of (red-black-tree-iterate-least/>=? t₅ 7)) 10
     (val-of (red-black-tree-iterate-least/>=? t₅ 10)) 10
     #:! #:t (red-black-tree-iterate-least/>=? t₅ 11)

     (val-of (red-black-tree-iterate-greatest/<=? t₅ 11)) 10
     (val-of (red-black-tree-iterate-greatest/<=? t₅ 10)) 10
     (val-of (red-black-tree-iterate-greatest/<=? t₅ 7)) 5
     (val-of (red-black-tree-iterate-greatest/<=? t₅ 5)) 5
     (val-of (red-black-tree-iterate-greatest/<=? t₅ 2)) 1
     (val-of (red-black-tree-iterate-greatest/<=? t₅ 1)) 1
     #:! #:t (red-black-tree-iterate-greatest/<=? t₅ 0)
     ))

  (test-case "dict basic"
    (chk
     (dict->list t₁) '()
     (dict->list t₄) '((a . 1) (b . 2) (c . 3))
     ))

  (define alphabet
    '(foo bar baz qux quux anteater bear cat dog elephant fox))
  (define (test-dict-correct keys sets removes)
    (define-values (t d)
      (for/fold ([t (red-black-tree)]
                 [d (hash)])
                ([_ (in-range sets)])
        (define k (random-ref keys))
        (define v (random 100))
        (values (red-black-tree-set t k v)
                (hash-set d k v))))
    (define-values (t* d*)
      (for/fold ([t t]
                 [d d])
                ([_ (in-range removes)])
        (define k (random-ref keys))
        (values (red-black-tree-remove t (random-ref keys))
                (hash-remove d k))))
    (equal? d (make-immutable-hash (dict->list t))))

  (define TESTS 1000)
  (test-case "stress test"
    (for ([_ (in-range TESTS)])
      (check-true (test-dict-correct alphabet (random 50) (random 25)))))
  )
