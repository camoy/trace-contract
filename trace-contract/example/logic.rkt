#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dependencies

(module deps racket/base
  (provide (all-defined-out)
           (all-from-out racket/contract)
           (all-from-out trace-contract))
  (require racket/contract
           racket/class
           racket/list
           racket/sequence
           trace-contract)

  ;;
  ;; events
  ;;
  (struct @next (i) #:transparent)
  (struct @update (m) #:transparent)
  (struct @iterator (c i) #:transparent)
  (struct @create (m c) #:transparent)
  (struct @has-next (i) #:transparent)

  ;;
  ;; classes
  ;;
  (define map%
    (class object%
      (init elems)
      (super-new)
      (define current-hash (make-immutable-hash elems))
      (define/public (keys)
        (new collection% [elems (hash-keys current-hash)]))
      (define/public (set k v)
        (set! current-hash (hash-set current-hash k v)))))

  (define collection%
    (class object%
      (init elems)
      (super-new)
      (define current-elems elems)
      (define/public (iterator)
        (new iterator% [elems current-elems]))))

  (define iterator%
    (class object%
      (init elems)
      (super-new)
      (define current-elems elems)
      (define/public (next)
        (begin0
          (first current-elems)
          (set! current-elems (rest current-elems))))
      (define/public (has-next)
        (not (empty? current-elems)))))

  (define indexed-iterator%
    (class object%
      (init iter bad?)
      (super-new)
      (define current-iter iter)
      (define current-bad? bad?)
      (define index -1)
      (define/public (next)
        (set! index (add1 index))
        (when current-bad? (send current-iter next))
        (list index (send current-iter next)))
      (define/public (has-next)
        (send current-iter has-next))))

  ;;
  ;; functions
  ;;
  (define (new-iterator seq)
    (new iterator% [elems (sequence->list seq)]))

  (define (with-index iter bad?)
    (new indexed-iterator% [iter iter] [bad? bad?]))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; has next property (local)

(module has-next-local racket/base
  (require (submod ".." deps)
           racket/class
           automata/re
           automata/re-ext
           automata/machine)

  (define iterator/c
    (object-trace/c
     #:satisfies (re (star (seq 'has-next (opt 'next))))
     [next     (->m any/c)    'next]
     [has-next (->m boolean?) 'has-next]))

  (provide
   (contract-out
    [new-iterator (-> sequence? iterator/c)]
    [with-index (-> iterator/c boolean? iterator/c)]))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/contract
           racket/class
           racket/list
           racket/string
           (prefix-in has-next-local: (submod ".." has-next-local)))

  ;; Ensures that the given exception contains the given strings.
  (define ((exn-ok? party-ok? . strs) e)
    (define blm (exn:fail:contract:blame-object e))
    (and (party-ok? (blame-positive blm))
         (for/and ([s (in-list strs)])
           (string-contains? (exn-message e) s))))

  ;;
  ;; has next property (local)
  ;;
  (chk
   #:do (define t1 (has-next-local:new-iterator '(a b c)))
   (list (let ()
           (send t1 has-next)
           (send t1 next))
         (let ()
           (send t1 has-next)
           (send t1 next))
         (let ()
           (send t1 has-next)
           (send t1 next)))
   '(a b c)

   #:do (define t2 (has-next-local:new-iterator '(a b c)))
   #:do (send t2 has-next)
   (send t2 next)  'a
   #:x (send t2 next)
   (exn-ok? (λ (p) (eq? (second p) 'test))
            "next: contract violation"
            "contract on: new-iterator")

  #:do (define t3 (has-next-local:new-iterator '(a b c)))
  #:do (define t4 (has-next-local:with-index t3 #t))
  #:do (send t4 has-next)
  #:x (send t4 next)
  (exn-ok? (λ (p) (eq? (second p) 'has-next-local))
           "next: contract violation"
           "contract on: with-index")

  #:do (define t5 (has-next-local:new-iterator '(a b c)))
  #:do (define t6 (has-next-local:with-index t5 #t))
  #:x (send t6 next)
  (exn-ok? (λ (p) (eq? (second p) 'test))
           "next: contract violation"
           "contract on: with-index")
  ))
