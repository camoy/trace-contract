#lang racket

;; ---------------------------------------------------------------------------------------------------

(provide main)

(require "admin/referee.rkt"
         "admin/manager.rkt"
         "player/player.rkt"
         "player/greedy.rkt"
         "common/game-state.rkt"
         "common/player-interface.rkt"
         "common/referee-interface.rkt"
         "../util/measure.rkt"
         "../util/deps/swdev/Testing/communication.rkt"
         "../util/deps/swdev/Lib/pattern-matching.rkt"
         "../util/deps/swdev/Testing/testing.rkt")

;; ---------------------------------------------------------------------------------------------------

(define T# 100)
(define ILL "ill-formed JSON")
(define INV "invalid JSON: ")

(define-modifiable
  #:level base
  (define done (λ () (for ([_ 65873]) (log-info "heisenberg"))))

  #:level noop
  (define done void)

  #:level check
  (define done void))

(define (main data)
  (for ([k (in-naturals)]
        [datum (in-list data)])
    (match-define (list in out) datum)
    (r-check-equal? do-test (list in) (list out) (format "(~a)" k)))
  (done))

(define (do-test)
  (define run (get game-description 'xref ""))
  (define result (run))
  (send-message (first result)))

;; read a JSON value from current input port, validate and map to internal data ~~ or (error tag msg)
(define (get validator tag msg)
  (define x (read-message))
  (cond
    [(eof-object? x) (error tag "missing JSON")]
    [(and (string? x) (regexp-match #px"ERROR" x)) (error tag "~a:\n ~a" ILL x)])
  (define pieces (call-with-values (λ () (validator x)) list))
  (unless (first pieces) (error tag "~a ~a: ~e" INV msg x))
  (apply values pieces))

#; {JSexpr -> False or (-> [List Ranking [Listof Player]])}
(define (game-description j)
  ;; the input pattern
  (def/mp object (_ r c p f d)
    #'(hash-table
       ('row     (? natural? r))
       ('column  (? natural? c))
       ('fish    (? natural? f))
       ('players (list (list (? string? p) d) (... ...)))))
  ;; go:
  (match j
    [(object r c p f dx) (run r c p f dx)]
    [_ #false]))

#; {N N N [Listof String : ordered] N N -> [-> Rankings]}
;; create a thunk that creates
;; -- one player per name in `names`, each using the greedy strategy,
;; -- runs the tournament
(define ((run r c names f depths))
  (define players (map (λ (n d) (make-player n strategy%)) names depths))
  (define named-p (map list players names))
  (match-define `[,firsts ,failed]
    (cond
      [(<= (length players) 4)
       (manager players #:fixed f #:size (list r c))]
      [else
       (for ([i (in-range T#)])
         (manager players #:fixed f #:size (list r c)))
       (manager players #:fixed f #:size (list r c))]))

  (when (cons? failed) `[timed out players: ,failed])
  (list
   (sort (for/list ([p firsts]) (second (assoc p named-p))) string<=?)
   (sort (for/list ([p failed]) (second (assoc p named-p))) string<=?)))

;; ---------------------------------------------------------------------------------------------------

(define-main-module
  #:entry-point main
  #:data "data/fish-hist.rktd")
