#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         "util/test.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol

;; Predicate that checks the Take5 protocol.
(define (protocol? t n)
  (stream~> t
            #:take (take* _ n)
            (start-round-phase? _ n)
            #:take (take* _ n)
            (start-turn-phase? _ n)
            #:take (takef _ choose?)
            (choose-phase? _ n)
            #:repeat))

;; Returns if the start round phase is correct.
(define (start-round-phase? xs n)
  (define methods (map second xs))
  (define objs (map first xs))
  (and (subset? methods '(start-round))
       (unique? objs)))

;; Returns if the start turn phase is correct.
(define (start-turn-phase? xs n)
  (define methods (map second xs))
  (define objs (map first xs))
  (and (subset? methods '(start-turn))
       (unique? objs)))

;; Returns if the choose phase is correct.
(define (choose-phase? xs n)
  (define objs (map first xs))
  (unique? objs))

;; Predicates to recognize the phase of an event.
(define ((make-second-eq? s) p) (eq? (second p) s))
(define start-round? (make-second-eq? 'start-round))
(define start-turn? (make-second-eq? 'start-turn))
(define choose? (make-second-eq? 'choose))

;; Groups together events by their game.
(define (stream-group-by-game games t)
  (define h (make-immutable-hash (stream->list games)))
  (define lengths (hash-map-values (hash-invert h) length))
  (define groups (stream-group-by t (λ (p) (hash-ref h (first p)))))
  (for/list ([group (in-list groups)])
    (define rep (first (stream-first group)))
    (list group (hash-ref lengths (hash-ref h rep)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

;; Maps a function over all the values of a hash.
(define (hash-map-values h f)
  (for/hash ([(k v) (in-hash h)])
    (values k (f v))))

;; Reverses the keys and values of a hash.
(define (hash-invert h)
  (for/fold ([r (hash)])
            ([(k v) (in-hash h)])
    (hash-update r
                 v
                 (λ (ks) (cons k ks))
                 (λ () null))))

;; The same as `take`, but more lax in that it doesn't raise an error the amount
;; to take is larger than the length of the list.
(define (take* xs k)
  (if (< (length xs) k)
      xs
      (take xs k)))

;; Returns if the list of elements is unique.
(define (unique? xs)
  (for/fold ([res #t]
             [s (set)]
             #:result res)
            ([x (in-list xs)])
    (values (and res (not (set-member? s x)))
            (set-add s x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define (sequence-ok? games moves)
  (for/and ([group (in-list (stream-group-by-game games moves))])
    (match-define (list t n) group)
    (protocol? t n)))

(define player/c
  (trace/c ([games any/c] [moves any/c])
    (class/c
     [join-game (->dm ([game (list/t this games)]) any)]
     [start-round (-> (list/t moves 'start-round) any)]
     [start-turn (-> (list/t moves 'start-turn) any)]
     [choose (-> (list/t moves 'choose) any)])
    (full (games moves) sequence-ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do
   (define/contract (make-player%)
     (-> player/c)
     (class object%
       (super-new)
       (define/public (join-game game) (void))
       (define/public (start-round) (void))
       (define/public (start-turn) (void))
       (define/public (choose) (void))
       ))

   #:do
   (define-values (p1 p2 g)
     (let ([player% (make-player%)])
       (values (new player%)
               (new player%)
               (gensym 'game))))

   #:t (begin
         (send p1 join-game g)
         (send p2 join-game g)
         (send p1 start-round)
         (send p2 start-round)
         (send p1 start-turn)
         (send p2 start-turn)
         (send p1 choose)
         (send p1 start-round)
         (send p2 start-round)
         (send p1 start-turn)
         (send p2 start-turn)
         (send p1 start-round)
         (send p2 start-round)
         (send p1 start-turn)
         (send p2 start-turn))

   ;; Too many `start-turn`
   #:x
   (send p2 start-turn)
   (trace-exn? (current-contract-region)
               '(function make-player%))

   #:do
   (define-values (p3 p4)
     (let ([player% (make-player%)])
       (values (new player%) (new player%))))

   #:t (begin
         (send p3 join-game g)
         (send p4 join-game g)
         (send p3 start-round))

   ;; Non-unique `start-round`
   #:x
   (send p3 start-round)
   (trace-exn? (current-contract-region)
               '(function make-player%))
   ))
