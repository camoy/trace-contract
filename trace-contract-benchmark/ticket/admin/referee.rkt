#lang racket

;; a referee for supervising one game of Trains

;; TODO:
;; -- at most 8 players, at least 2 distinct players

;; The referee kicks out
;; -- cheating players
;; -- players that violate the (logical) contract in `Common/` (exn:fail:contract)
;; -- players that raise an exception (exn:fail)
;; -- players that take "too long" (accidental infinite loops)


;; The referee is abstract over two pieces to facilitate testing:
;; -- in what order to select (enough) destinations from the map for players to pick from;
;; -- in what order need a systematic way to hand cards to players.

;
;
;
;                                             ;
;                                             ;
;    ;;;;   ;   ;;  ; ;;;    ;;;;    ;;;;;  ;;;;;;   ;;;;
;    ;  ;;   ;  ;   ;;  ;   ;;  ;;   ;;       ;     ;    ;
;   ;    ;    ;;    ;    ;  ;    ;   ;        ;     ;
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;;;
;   ;         ;;    ;    ;  ;    ;   ;        ;        ;;;
;   ;         ;;    ;    ;  ;    ;   ;        ;          ;
;    ;       ;  ;   ;;  ;   ;;  ;;   ;        ;     ;    ;
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(provide
 ERR

 #; {type [RefereeResult X] = (List [Listof [Listof X]] [Listof X]) }

 #; {[[Listof XPlayer] GameMap
                       ;; the next two optional parameters are for deterministic testing
                       #:cards [Listof Card]
                       #:shuffle [[Listof Destination] -> [Listof Destination]]
                       ->
                       (U ERR [RefereeResult XPlayer])]}
 (contract-out
  [referee actual-referee/c]))

(module+ examples ;; from referee to manager
  (provide
#|
   #; RefereePlayerClass
   mock%

   #; {[RefereeResult XPlayer]-> [RefereeResults String]}
   ref-results->names

   #; {GameMap N N N -> [List [Listof XPlayer/Hold-10][Listof XPlayer/NuyNow][Listof XPlayer/Cheats]]}
   make-players
|#
   #; {[Listof XPlayer] -> [Listof String]}
   ;; extract nanes, sort by string<?
   players->names
#|
   #; {GameMap from "map-1.json"}
   big-map

   #; {[Listof Destination] -> [Listof Destination]}
   sorted-destinations
|#))

#;(module+ examples ;; examples for setup, turns, and scoring
  (provide mock-more-card ii-default make-an-ii cards lop))

;
;
;        ;                                       ;                             ;
;        ;                                       ;                             ;
;        ;                                       ;
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ;;;;;;   ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;  ;  ;  ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ;
;   ;    ;  ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;  ;  ; ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;
;   ;    ;  ;       ;    ;  ;       ;  ;  ; ;    ;  ;       ;    ;  ;          ;    ;          ;;;
;   ;    ;  ;       ;    ;  ;       ;  ;  ; ;    ;  ;       ;    ;  ;          ;    ;            ;
;    ;  ;;   ;      ;;  ;    ;      ;  ;  ;  ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ;
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;  ;  ;  ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(require "../../util/measure.rkt")
(require "../common/basic-constants.rkt")
(require "../common/map.rkt")
(require "../common/state.rkt")
(require "state.rkt")
(require "../lib/xsend.rkt")
(require "../common/referee-interface.rkt")

#;(module+ examples
  (require (submod "../common/map.rkt" examples))
  (require (submod "../common/state.rkt" examples))
  (require "../common/map-serialize.rkt")
  (require (prefix-in hold-10: "../player/hold-10-strategy.rkt"))
  (require (prefix-in buy-now: "../player/buy-now-strategy.rkt"))
  (require (prefix-in cheat:   "../player/cheat-strategy.rkt"))
  (require "../player/astrategy.rkt")
  (require "../player/player.rkt")
  (require racket/runtime-path))

#;(module+ test
  (require (submod ".." examples))
  (require (submod "../common/map.rkt" examples))
  (require (submod "../common/state.rkt" examples))
  (require "../common/connection.rkt")
  (require rackunit))

;
;
;                                            ;;;
;                                              ;
;                                              ;
;    ;;;;   ;   ;;    ;;;   ;;;;;;  ; ;;;      ;     ;;;;    ;;;;
;    ;  ;;   ;  ;    ;   ;  ;  ;  ; ;;  ;      ;     ;  ;;  ;    ;
;   ;    ;    ;;         ;  ;  ;  ; ;    ;     ;    ;    ;  ;
;   ;;;;;;    ;;     ;;;;;  ;  ;  ; ;    ;     ;    ;;;;;;  ;;;
;   ;         ;;    ;    ;  ;  ;  ; ;    ;     ;    ;          ;;;
;   ;         ;;    ;    ;  ;  ;  ; ;    ;     ;    ;            ;
;    ;       ;  ;   ;   ;;  ;  ;  ; ;;  ;      ;     ;      ;    ;
;    ;;;;;  ;    ;   ;;; ;  ;  ;  ; ; ;;;       ;;;  ;;;;;   ;;;;
;                                   ;
;                                   ;
;                                   ;
;

#;(module+ examples ;; examples for referee, setup, turns, and scoring
  (define (mock% #:setup (w void) #:pick (p values) #:play (y values) #:more (x void) #:win (z void))
    (class object% (init-field [strategy% 0] [name (gensym 'player)])
      (define/public (setup . x) (w x))
      (define/public (pick x) (p (apply set (take (set->list x) (- PICKS-PER DESTS-PER)))))
      (define/public (more cards) (x cards))
      (define/public (play . x) (y vtriangle-boston-seattle))
      (define/public (win . x) (z x))
      (super-new)))

  (define mock-more-card (new [mock% #:play (λ _ MORE)]))

  (define ([make-an-ii mock-inst] #:rails (rails 3) #:con (con '()) . cards)
    (ii '[x z] '[y z] rails (apply hash cards) (apply set con) mock-inst))
  (define ii-default  (make-an-ii (new (mock%))))

  (define (lop cards-p xternal (cards-f '[]))
    (list (ii+payload (ii+cards ii-final cards-f) mock-more-card)
          (ii+payload (ii+cards ii-play cards-p) xternal)))
  (define cards '[red red]))

;
;
;                      ;;;
;                     ;
;                     ;
;    ;;;;;   ;;;;   ;;;;;;   ;;;;    ;;;;;   ;;;;    ;;;;
;    ;;      ;  ;;    ;      ;  ;;   ;;      ;  ;;   ;  ;;
;    ;      ;    ;    ;     ;    ;   ;      ;    ;  ;    ;
;    ;      ;;;;;;    ;     ;;;;;;   ;      ;;;;;;  ;;;;;;
;    ;      ;         ;     ;        ;      ;       ;
;    ;      ;         ;     ;        ;      ;       ;
;    ;       ;        ;      ;       ;       ;       ;
;    ;       ;;;;;    ;      ;;;;;   ;       ;;;;;   ;;;;;
;
;
;
;

(define ERR "error: not enough destinations")

(define (referee the-external-players the-game-map
                 #; [Listof Card]
                 #:cards   (cards (make-list 100 'red))
                 #; [[Listof Destination] -> [Listof Destination]]
                 #:shuffle (shuffle values))

  (time-out-limit 2.0)

  (define destination* (shuffle (all-destinations the-game-map)))

  ;; is this too expensive as a contract?
  (cond
    [(< (length destination*) (+ (- PICKS-PER DESTS-PER) (* DESTS-PER (length the-external-players))))
     ERR]
    [else
     (let* ([the-state (setup-all-players the-external-players the-game-map cards destination*)]
            [the-state (play-turns the-state the-game-map)]
            [results   (score-game the-state the-game-map)])
       results)]))

#;(module+ test
  (define-values (m1 m2) (values (new (mock%)) (new (mock%))))
  (check-equal? (referee [list m1 m2] vtriangle) ERR)
  (check-equal? (referee [list m1 m2] vrectangle) `[ () [,m2 ,m1]]))

;
;
;
;                     ;
;                     ;
;    ;;;;    ;;;;   ;;;;;;          ;    ;  ; ;;;
;   ;    ;   ;  ;;    ;             ;    ;  ;;  ;
;   ;       ;    ;    ;             ;    ;  ;    ;
;   ;;;     ;;;;;;    ;             ;    ;  ;    ;
;      ;;;  ;         ;             ;    ;  ;    ;
;        ;  ;         ;             ;    ;  ;    ;
;   ;    ;   ;        ;             ;   ;;  ;;  ;
;    ;;;;    ;;;;;     ;;;           ;;; ;  ; ;;;
;                                           ;
;                                           ;
;                                           ;
;

#; {[Listof XPlayer] Map [Listof Card] [Listof Desinations] -> RefereeState}
;; inform players about the beginning of the game: opponents, rails, colored cards
;; separate drop-outs from  active to passive state
(define (setup-all-players externals0 game-map cards0 destinations0)
  (let loop ([externals externals0] [Cs cards0] [Ds destinations0] [good '()] [drop-outs '()])
    (match externals
      ['() (rstate (reverse good) Cs drop-outs)] ;; age!
      [(cons xplayer others)
       (define cards (take Cs CARD0-PER))
       (match (xsend xplayer setup game-map RAILS-PER cards)
         [(? failed?) (loop others Cs Ds good (cons xplayer drop-outs))]
         [_ (define pick-from (apply set (take Ds PICKS-PER)))
            (match (xsend xplayer pick pick-from)
              [(? failed?) (loop others Cs Ds good (cons xplayer drop-outs))]
              [rejects
               (match (legal-picks pick-from rejects Ds)
                 [#false (loop others Cs Ds good (cons xplayer drop-outs))]
                 [(list (list d-1 d-2) remaining)
                  (define iplayer (ii d-1 d-2 RAILS-PER (->hash cards) (set) xplayer))
                  (loop others (drop Cs CARD0-PER) remaining (cons iplayer good) drop-outs)])])])])))

#; {[Setof Destination] [Setof Destination] [Listof Destination]
                        ->
                        (U False [List [List Destination Destination] [Listof Destination]])}
;; determine whether the rejection is legitimate, compute the chosen ones and the remaining ones
(define (legal-picks choose-from rejected all)
  (cond
    [(not (subset? rejected choose-from))                   #false]
    [(not (= (set-count rejected) (- PICKS-PER DESTS-PER))) #false]
    [else
     (define chosen-ones (set->list (set-subtract choose-from rejected)))
     (define remaining   (remove* chosen-ones all))
     (list chosen-ones remaining)]))

(define (->hash loc)
  (for/fold ([h (hash)]) ([c loc])
    (hash-update h c add1 0)))


;
;
;
;     ;                       ;
;     ;                       ;
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;      ;  ;;  ;    ;    ;     ;    ;
;     ;     ;    ;  ;         ;     ;
;     ;     ;;;;;;  ;;;       ;     ;;;
;     ;     ;          ;;;    ;        ;;;
;     ;     ;            ;    ;          ;
;     ;      ;      ;    ;    ;     ;    ;
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;


#;(module+ test

  ;; -------------------------------------------------------------------------------------------------
  ;; set-up
  (define mock-bad-set   (new (mock% #:setup (λ _ (raise 'bad)))))
  (define vtriangle-apick (apply set (all-destinations vtriangle)))
  (define mock-good-pick (new (mock% #:pick (λ _ vtriangle-apick))))
  (define mock-bad-pick  (new (mock% #:pick (λ _ (raise 'bad)))))
  (define mock-ill-pick  (new (mock% #:pick (λ _ (set)))))

  (define setup-cards (make-list (* 2 #;players CARD0-PER) 'red))
  (define (make-ii-set ds)
    (ii (car ds) (cadr ds) RAILS-PER (->hash (make-list CARD0-PER 'red)) (set) mock-good-pick))
  (define-values (1dest 2dest)
    (let* ([dests (reverse vtriangle-dests)]
           [2dest (reverse (take dests DESTS-PER))]
           [dests (drop dests DESTS-PER)]
           [1dest (reverse (take dests DESTS-PER))])
      (values 1dest 2dest)))

  (check-equal? (setup-all-players (list mock-bad-set) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-bad-set)))
  (check-equal? (setup-all-players (list mock-ill-pick) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-ill-pick)))
  (check-equal? (setup-all-players (list mock-bad-pick) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-bad-pick)))

  (define 2goodies (list mock-good-pick mock-good-pick))
  (check-equal? (setup-all-players 2goodies vtriangle setup-cards vtriangle-dests)
                (rstate (list (make-ii-set 1dest) (make-ii-set 2dest)) '[] '[]))


  ;; -------------------------------------------------------------------------------------------------
  ;; legal-picks
  (check-equal?
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC] '[NYC DC])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando]))
   '[[[Boston Chicago] [Boston Orlando]] ([Boston DC] [Boston NYC] [NYC DC] [DC Orlando])])

  (check-false
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC] '[NYC Boston])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando])))

  (check-false
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando]))))

;
;
;
;     ;
;     ;
;   ;;;;;;  ;    ;   ;;;;;  ; ;;;    ;;;;
;     ;     ;    ;   ;;     ;;   ;  ;    ;
;     ;     ;    ;   ;      ;    ;  ;
;     ;     ;    ;   ;      ;    ;  ;;;
;     ;     ;    ;   ;      ;    ;     ;;;
;     ;     ;    ;   ;      ;    ;       ;
;     ;     ;   ;;   ;      ;    ;  ;    ;
;      ;;;   ;;; ;   ;      ;    ;   ;;;;
;
;
;
;

#; {RefereeState GameMap -> RefereeState}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
;; ASSUME all players have >= RAILS-MIN rails
(define (play-turns the-state0 gm)
  (let play ([the-state the-state0][turns-w/o-change 0])
    (define players (rstate-players the-state))
    (match players
      ['() the-state]
      [_ (match (play-1-turn the-state gm)
           [#false (play (rstate-drop the-state) 0)]
           [(list nu-the-state game-over?)
            (define no-change (if (equal? the-state nu-the-state) (add1 turns-w/o-change) 0))
            (define next-state (rstate-rotate nu-the-state))
            (if (or game-over? (= no-change (length players)))
                (play-last-round next-state gm)
                (play next-state no-change))])])))

#; {RefereeState GameMap -> [List RefereeState Boolean]}
;; allow each player to play one more turn, except the first one
(define (play-last-round the-state0 gm)
  (for/fold ([the-state (rstate-rotate the-state0)]) ((_ (rest (rstate-players the-state0))))
    (match (play-1-turn the-state gm)
      [#false       (rstate-drop the-state)]
      [(list nup _) (rstate-rotate nup)])))

#; {RefereeState GameMap -> (U False [List RefereeState Boolean])}
(define (play-1-turn the-state gm)
  (define next (first (rstate-players the-state)))
  (define the-player-state (rstate->pstate the-state))
  (match (xsend (ii-payload next) play the-player-state)
    [(? failed?) #false]
    [(? (curry equal? MORE))
     (if-rstate-update the-state (play-1-more-cards next (rstate-cards the-state)))]
    [response
     (and
      (legal-action? the-player-state gm response)
      (if-rstate-update the-state (player-1-acquire next response)))]))

#; {MePlayer [Listof Card] -> (U False [List MePlayer {listof Card}])}
(define (play-1-more-cards next remaining)
  (cond
    [(< (length remaining) CARDS-PER) (list next '[])]
    [else (define new-cards (take remaining CARDS-PER))
          (and (not (failed? (xsend (ii-payload next) more new-cards)))
               (list (ii+cards next new-cards) new-cards))]))

#; {MePlayer Response -> (U False [Listf MePlayer Boolean])}
(define (player-1-acquire next response)
  (let* ([iplayer next]
         [iplayer (ii-acquire iplayer response)])
    (list iplayer (ii-final? iplayer))))

;
;
;
;     ;                       ;
;     ;                       ;
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;      ;  ;;  ;    ;    ;     ;    ;
;     ;     ;    ;  ;         ;     ;
;     ;     ;;;;;;  ;;;       ;     ;;;
;     ;     ;          ;;;    ;        ;;;
;     ;     ;            ;    ;          ;
;     ;      ;      ;    ;    ;     ;    ;
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;

#;(module+ test ;; tests for turns

  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-acquire
  (check-equal? (player-1-acquire ii-play vtriangle-boston-seattle) (list ii-final #t))

  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-more-cards
  (define ii-bad-more (make-an-ii (new [mock% #:more (λ _ (raise 'bad))])))
  (check-equal? (play-1-more-cards (ii-default) cards) (list (ii-default 'red 2) cards))
  (check-equal? (play-1-more-cards (ii-default) (list 'red)) (list (ii-default) '[]))
  (check-false  (play-1-more-cards (ii-bad-more) cards))

  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-turn
  (define active1 (ii-default 'red 3))
  (check-equal?
   (play-1-turn (rstate (list active1) '[] '[]) vtriangle)
   (list (rstate (list (ii-default #:rails 0 #:con `[,vtriangle-boston-seattle])) '[] '[]) #true))
  (define active2 (ii-default))
  (check-false (play-1-turn (rstate (list active2) '[] '[]) vtriangle))

  (define active3 [(make-an-ii (new [mock% #:play (λ _ (raise 'bad))]))])
  (check-false (play-1-turn (rstate (list active3) '[] '[]) vtriangle))

  (define active4 (make-an-ii (new [mock% #:play (λ _ MORE)])))
  (check-equal?
   (play-1-turn (rstate (list [active4]) cards '[]) vtriangle)
   (list (rstate (list (active4 'red 2)) '[] '[]) #false))

  ;; -------------------------------------------------------------------------------------------------
  ;; play-last-round
  (define mock-bad-play  (new [mock% #:play (λ _ (raise 'bad-o))]))

  (define lop-ask-more (lop '[] mock-more-card))
  (define lop-got-more (lop cards mock-more-card))
  (define lop-bad-play (lop '[] mock-bad-play))

  (check-equal? (play-last-round (rstate lop-ask-more cards '()) vtriangle)
                (rstate lop-got-more '[] '[]))
  (check-equal? (play-last-round (rstate lop-bad-play '[] '()) vtriangle)
                (rstate (list (first lop-bad-play)) '[] `[,(ii-payload (second lop-bad-play))]))

  ;; -------------------------------------------------------------------------------------------------
  ;; play-turns
  (check-equal? (play-turns (rstate (list (ii+payload ii-play mock-more-card)) cards '()) vtriangle)
                (rstate (list (ii+cards (ii+payload ii-play mock-more-card) cards)) '[] '()))

  (define lop-turn-got-more (reverse (lop '[] mock-more-card cards)))
  (define lop-turn-bad-play (list (ii+payload ii-final mock-bad-play)))

  (check-equal? (play-turns (rstate lop-ask-more cards '()) vtriangle)
                (rstate lop-turn-got-more '[] '[]))
  (check-equal? (play-turns (rstate '[]  cards '()) vtriangle)
                (rstate '[] cards '()))
  (check-equal? (play-turns (rstate lop-turn-bad-play cards '()) vtriangle)
                (rstate '[] cards (map ii-payload lop-turn-bad-play))))

;
;
;                                      ;
;
;
;    ;;;;     ;;;    ;;;;    ;;;;    ;;;    ; ;;;    ;;; ;
;   ;    ;   ;   ;  ;;  ;;   ;;  ;     ;    ;;   ;  ;;  ;;
;   ;       ;       ;    ;   ;         ;    ;    ;  ;    ;
;    ;;;;   ;       ;    ;   ;         ;    ;    ;  ;    ;
;        ;  ;       ;    ;   ;         ;    ;    ;  ;    ;
;   ;    ;   ;   ;  ;;  ;;   ;         ;    ;    ;  ;;  ;;
;    ;;;;     ;;;    ;;;;    ;       ;;;;;  ;    ;   ;;; ;
;                                                        ;
;                                                    ;  ;;
;                                                     ;;;
;


#; {Yype Rankings = [Listof [Listof XPlayer]]}

#; {RefereeState Map -> [List Ranking [Listof XPlayer]]}
;; compute ranking from state, inform players of winning/losing
;; separate drop-outs from  active to passive state
(define (score-game the-state game-map)
  (match (rstate-players the-state)
    ['() (list '[] (rstate-drop-outs the-state))]
    [players
     (define +scored
       (let* ([+scored (map (λ (p) `(,p 0 ,(project-game-map game-map (ii-connections p)))) players)]
              [+scored (score-connections +scored)]
              [+scored (score-longest-path +scored)]
              [+scored (score-destinations +scored)])
         +scored))
     (define ranking (rank +scored))
     ;; --- rank and inform --
     (xinform ranking (rstate-drop-outs the-state))]))

#; {type Scored = [Listof (List PlayerState N GameMap)]}

#; {Scored -> Scored}
(define (score-connections +scored)
  (for/list ([p.s +scored])
    (match-define (list p s gmp) p.s)
    (list p (ii-conn-score p) gmp)))

#; {Scored -> Scored}
(define (score-destinations +scored)
  (for/list ([p.s +scored])
    (match-define (list p s gm) p.s)
    (list p (+ s (ii-destinations-connected p gm)) gm)))

#; {Scored -> Scored}
(define (score-longest-path +scored)
  (define players.longest-path
    (for/list ([p.s +scored])
      (match-define (list p s gm) p.s)
      (cons p.s (if (set=? (ii-connections p) (set)) 0 (game-map-longest-path gm)))))
  (define the-longest (apply max (map cdr players.longest-path)))
  (cond
    [(zero? the-longest) +scored]
    [else
     (for/fold ([result '()]) ([p+s players.longest-path])
       (match-define (cons (and p.s (list p s gm)) longest-for-p) p+s)
       (if (= longest-for-p the-longest)
           (cons (list p (+ s LONG-PATH) gm) result)
           (cons p.s result)))]))

#; {Scored -> Ranking}
(define (rank +scored)
  (define sorted (sort +scored > #:key second))
  (define grouped (group-by second sorted))
  (for/list ([group grouped])
    (for/list ([p.s group])
      (ii-payload (first p.s)))))

#; {Ranking [Listof XPlayer] -> [List Ranking [Listof XPlayer]]}
(define (xinform rankings drops)
  (define (if-cons a d) (if (empty? a) d (cons a d)))
  (match rankings
    ['() (list rankings drops)]
    [(cons prelim-winners losers)
     (define-values (goods bads) (xmap-send (λ (a) (xsend a win #true)) prelim-winners))
     (let loop ([losers losers] [ranks (if-cons (map first goods) '[])] [drops (append bads drops)])
       (match losers
         ['() (list (reverse ranks) drops)]
         [(cons group others)
          (define-values (goods bads) (xmap-send (λ (a) (xsend a win #false)) group))
          (loop others (if-cons (map first goods) ranks) (append bads drops))]))]))

;
;
;
;     ;                       ;
;     ;                       ;
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;      ;  ;;  ;    ;    ;     ;    ;
;     ;     ;    ;  ;         ;     ;
;     ;     ;;;;;;  ;;;       ;     ;;;
;     ;     ;          ;;;    ;        ;;;
;     ;     ;            ;    ;          ;
;     ;      ;      ;    ;    ;     ;    ;
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;

#;(module+ test

  (define projected (map (λ (p) (project-game-map vtriangle (ii-connections p))) lop-ask-more))

  (define lop2+score
    (list (list (first lop-ask-more)  8 (first projected))
          (list (second lop-ask-more) 5 (second projected))))
  (define lop3+score
    (list (list (first lop-ask-more)  (+ 8 (* 2 POINTS-PER)) (first projected))
          (list (second lop-ask-more) (- 5 (* 2 POINTS-PER)) (second projected))))
  (define lop4+score
    (list (list (first lop-ask-more)  (+ 8 LONG-PATH (* 2 POINTS-PER)) (first projected))
          (list (second lop-ask-more) (- 5 (* 2 POINTS-PER))           (second projected))))
  (define lop4-ranked
    [list (list (ii-payload (first (first lop4+score))))
          (list (ii-payload (first (second lop4+score))))])

  (define projected+ (map (λ (p gm) (list p 0 gm)) lop-ask-more projected))

  ;; -------------------------------------------------------------------------------------------------
  ;; score connections owned by players
  (check-equal? (score-connections projected+) lop2+score)

  ;; -------------------------------------------------------------------------------------------------
  ;; score destinations of players

  (check-equal? (score-destinations lop2+score) lop3+score)

  ;; -------------------------------------------------------------------------------------------------
  ;; score longest path
  (check-equal? (score-longest-path lop3+score) (reverse lop4+score))

  (define p1-beats-p2
    (let ()
      (define basic  (set [connection Boston Seattle green 4]))
      (define better (set [connection Boston Seattle red 3][connection Orlando Seattle blue 5]))
      (define p1 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) better 'x))
      (define p2 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) basic  'y))
      (define p1-p (project-game-map vtriangle better))
      (define p2-p (project-game-map vtriangle basic))
      (define p1-beats-p2 `[(,p2 0 ,p2-p) (,p1 ,LONG-PATH ,p1-p)])

      (check-equal? (score-longest-path `[(,p2 0 ,p2-p) (,p1 0 ,p1-p)]) (reverse p1-beats-p2))

      p1-beats-p2))

  (define _
    (let ()
      (define p1 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) (set) 'x))
      (define p2 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) (set) 'y))
      (define p1-p (project-game-map vtriangle (set)))
      (define p2-p (project-game-map vtriangle (set)))
      (define p1-p2 `[(,p2 0 ,p2-p) (,p1 0 ,p1-p)])

      (check-equal? (score-longest-path `[(,p2 0 ,p2-p) (,p1 0 ,p1-p)]) p1-p2 "no paths, no points")))

  ;; -------------------------------------------------------------------------------------------------
  ;; rank players
  (define (ranked-ii i) (list (ii '[x z] '[y z] 0 (hash) (set) (~a i)) (- 3 i) '_))
  (check-equal? (rank (cons (ranked-ii 2) (build-list 3 ranked-ii))) '[ ["0"] ["1"] ["2" "2"] ])
  (check-equal? (rank lop4+score) lop4-ranked)

  (check-equal? (rank p1-beats-p2) '[[x] [y]])

  ;; -------------------------------------------------------------------------------------------------
  ;; inform winners, eliminate silly losers
  (check-equal? (xinform lop4-ranked '[]) [list lop4-ranked '()])
  (check-equal? (xinform '() '[]) [list '() '()])

  (define fail-on-win (new (mock% #:play values #:win (λ _ (raise 'bad)))))
  (check-equal? (xinform (cons (list fail-on-win) lop4-ranked) '[]) `[,lop4-ranked (,fail-on-win)])

  ;; -------------------------------------------------------------------------------------------------
  ;; `score-game`
  (check-equal? (score-game (rstate lop-ask-more '() '()) vtriangle) [list lop4-ranked '[]]))

;
;
;      ;;;           ;;;     ;;;
;     ;                ;       ;              ;                       ;
;     ;                ;       ;              ;                       ;
;   ;;;;;;  ;    ;     ;       ;            ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;     ;    ;     ;       ;              ;      ;  ;;  ;    ;    ;     ;    ;
;     ;     ;    ;     ;       ;              ;     ;    ;  ;         ;     ;
;     ;     ;    ;     ;       ;              ;     ;;;;;;  ;;;       ;     ;;;
;     ;     ;    ;     ;       ;              ;     ;          ;;;    ;        ;;;
;     ;     ;    ;     ;       ;              ;     ;            ;    ;          ;
;     ;     ;   ;;     ;       ;              ;      ;      ;    ;    ;     ;    ;
;     ;      ;;; ;      ;;;     ;;;            ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;

#; {[RefereeResult XPlayer]-> [RefereeResults String]}
(define (ref-results->names result)
  (match-define [list rankings cheats] result)
  `[,(map players->names rankings) ,(players->names cheats)])

#; {[Listof XPlayer] -> [Listof String]}
(define (players->names players)
  (sort (map (λ (p) (~a (get-field name p))) players) string<?))

#;(module+ examples

  (define (make-players the-map hold-10# buy-now# cheat#)
    (define (make-players n strat% prefix)
      (build-list n
                  (λ (i)
                    (define name (~a prefix (make-string i #\a)))
                    (make-player #:strategy strat% #:gm the-map #:name name))))

    (define hold-10-players (make-players hold-10# hold-10:strategy% "holdten"))
    (define buy-now-players (make-players buy-now# buy-now:strategy% "buynow"))
    (define cheater-players (make-players cheat#   cheat:strategy%   "cheater"))

    `[,hold-10-players ,buy-now-players ,cheater-players])

  (define-runtime-path map1 "../data/map-1.json")
  (define big-map (with-input-from-file map1 read-and-parse-map))

  (define (sorted-destinations destinatuons) (sort destinatuons lexi<?)))

#;(module+ test

  ;; the numbers cannot be chosen freely
  ;; assumes that hold-10s are stupid, all buy-nows win

  (define (check-referee the-map hold-10# buy-now# cheat#)
    (match-define [list hold-10s buy-nows cheaters] (make-players the-map hold-10# buy-now# cheat#))
    (check-equal? (ref-results->names
                   (referee (append hold-10s buy-nows cheaters)
                            the-map
                            #:shuffle sorted-destinations
                            #:cards (make-list CARDS-PER-GAME 'white)))
                  (ref-results->names `{[[,@buy-nows] [,@hold-10s]] ,cheaters})))

  (check-referee vrectangle 1 1 1)
  (check-referee vrectangle 1 1 0)
  (check-referee big-map 7 1 0))

;; ---------------------------------------------------------------------------------------------------

(define-modifiable
  #:level base
  (define actual-referee/c referee/c)

  #:level noop
  (define actual-referee/c referee-no-load/c)

  #:level check
  (define actual-referee/c referee-load/c))
