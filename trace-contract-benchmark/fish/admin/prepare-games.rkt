#lang racket

;; allocate a list of items to a list of buckets of max/min size

#| items ~ players
The manager starts by assigning them to games with the maximal number of participants permitted.
Once the number of remaining players drops below the maximal number and can't form a game, the
manager backtracks one game and tries games of size one less than the maximal number and so on
until all players are assigned.

The function preserves the order of the players it is given.
|#

;
;
;                                                            ;;;
;                     ;                                        ;
;                     ;                                        ;
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;
;
;
;

(provide
 (contract-out
  [prepare-games
   (->i ([min-item-per-game natural?]
         [max-item-per-game (min-item-per-game) (and/c natural? (>/c min-item-per-game))]
         [players (min-item-per-game) (and/c list? (λ (l) (>= (length l) min-item-per-game)))])
        (r any/c)
        #:post/name (players r) "same order"
        (equal? (apply append r) players)
        #:post/name (min-item-per-game max-item-per-game players r) "proper sizes"
        (andmap (λ (l) (<= min-item-per-game (length l) max-item-per-game)) r))]))

;
;
;
;
;
;   ; ;;;    ;;;;    ;;;;   ; ;;;     ;;;    ;;;;    ;;;;
;   ;;  ;;   ;;  ;  ;    ;  ;;  ;;   ;   ;   ;;  ;  ;    ;
;   ;    ;   ;      ;;;;;;  ;    ;       ;   ;      ;;;;;;
;   ;    ;   ;      ;       ;    ;   ;;;;;   ;      ;
;   ;    ;   ;      ;       ;    ;  ;    ;   ;      ;
;   ;;  ;;   ;      ;;   ;  ;;  ;;  ;   ;;   ;      ;;   ;
;   ; ;;;    ;       ;;;;;  ; ;;;    ;;; ;   ;       ;;;;;
;   ;                       ;
;   ;                       ;
;   ;                       ;
;

(define (prepare-games min-item-per-game  max-item-per-game lop0)
  (define N (length lop0))
  (cond
    [(<= N max-item-per-game) (list lop0)]
    [else
     (define first-one (take lop0 max-item-per-game))
     (define remainder (drop lop0 max-item-per-game))
     (define n (- N max-item-per-game)) ;; so I don't have to run (length lop)
     ;; gen rec with accumulators n (remaining number) and players-per-game (ppg)
     (let loop ([lop remainder] [n n] [games `(,first-one)] [ppg max-item-per-game])
       (cond
         [(= n 0) ;; perfect
          (reverse games)]
         [(< n min-item-per-game) ;; backtrack
          (define one-prior (first games))
          (loop (append one-prior lop) (+ n ppg) (rest games) (- ppg 1))]
         [(< n ppg) ;; one small game
          (reverse (cons lop games))]
         [else ;; keep going
          (define next-game (take lop ppg))
          (define remaining (drop lop ppg))
          (loop remaining (- n ppg) (cons next-game games) ppg)]))]))
