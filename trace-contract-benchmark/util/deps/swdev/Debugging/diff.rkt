#lang racket

(provide
 #; {type Δ = (Δ Any Any Path)}
 #; {type P = '[] || (list* 'vector N P) || (list* 'struct... N P) || (list* 'hash N P)}
 
 #;{Any Any -> (U False Δ)}
 ;; Δ if there is a difference, #f is there isn't
 diff)

(struct Δ [left right path] #:transparent)

(require "spy.rkt")

(module+ test
  (require rackunit))

(define (diff tree1 tree2)

  (define (package x y Pth)
    (Δ x y (remove-vector-after-struct Pth)))
  
  (define (simple string=? x y path)
    (if (string=? x y) #f (package x y path)))

  (define (complex vector-length vector->list x y path)
    (or (if (= (vector-length x) (vector-length y)) #f (package x y path))
        (for/or ((a (vector->list x)) (b (vector->list y)) (i (in-naturals)))
          (diff a b (cons i path)))))

  (define (diff [tree1 tree1][tree2 tree2][path '()])
    (match* (tree1 tree2)
      [('() '()) #f]
      [((? number? x) (? number? y))   (simple = x y path)]
      [((? symbol? x) (? symbol? y))   (simple eq? x y path)]
      [((? string? x) (? string? y))   (simple string=? x y path)]
      [((? char? x)   (? char? y))     (simple eq? x y path)]
      [((? boolean? x) (? boolean? y)) (simple eq? x y path)]
      [((? vector? x) (? vector? y))   (complex vector-length vector->list x y (cons 'vector path))]
      [((? list? x)   (? list? y))     (complex length values x y (cons 'list path))]
      [((? struct? x) (? struct? y))
       (define xv (struct->vector x))
       (define yv (struct->vector y))
       (define x-tag (vector-ref xv 0))
       (define y-tag (vector-ref yv 0))
       (if (eq? x-tag y-tag) (diff (slice0 xv) (slice0 yv) (cons x-tag path)) (package x y path))]
      ;; let struct equality kick in first 
      [((? procedure? x)   (? procedure? y)) (simple eq? x y path)]
      [((? hash? x) (? hash? y))
       (complex (λ _ 0)
                (λ (h) (sort (hash-map h list) string<=? #:key (compose symbol->string first)))
                x y
                (cons 'hash path))]
      [((? set? x) (? set? y))
       (define x-y (set-subtract x y))
       (define y-x (set-subtract y x))
       (if (and (set-empty? x-y) (set-empty? y-x))
           #false
           (package x-y y-x (cons 'set path)))]
      [(_   _) (package tree1 tree2 path)]))

  (diff))

#; {P -> P}
(define (remove-vector-after-struct l)
  (cond
    [(empty? l) l]
    [(empty? (rest l)) l]
    [else
     (define pred (first l))
     (define-values (r _)
       (for/fold ([r `()][pred pred]) ([x (rest l)])
         (cond
           [(and (eq? 'vector pred) (regexp-match "struct" (~a x))) (values (cons x r) x)]
           [else (values (cons pred r) x)])))
     r]))

#; {Vector -> Vector}
(define (slice0 v)
  (list->vector (rest (vector->list v))))
  

(module+ test

  (diff (set 'a 'b) (set 'a))

  (check-false  (diff 1 1))
  (check-equal? (diff 1 2) (Δ 1 2 '()))

  (struct in  () #:transparent)
  (check-false (diff (in) (in)))

  (struct in2 [x] #:transparent)
  (check-equal? (diff [in2 '[1]] [in2 '[2]]) (Δ 1 2'[struct:in2 0 list 0]))

  (struct out ())
  (define x (out))
  (define y (out))
  (check-equal? (diff x y) (Δ x y '[])))

