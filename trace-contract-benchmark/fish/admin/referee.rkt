#lang racket

;; a referee that plays a single game with players given in the order of their "age"
;; and produces a ranking (list of players placed at same position) and a list of cheats

;; ---------------------------------------------------------------------------------------------------
;; This "bug switch" exists only for the trace-contract test. See trace-test.
;; In the second rouund, the bug will send a take-turn message to a third player out of order.
(provide enbug debug)

(define no-bug? #true)
(define (enbug) (set! no-bug? #false))
(define (debug) (set! no-bug? #true))

;; ---------------------------------------------------------------------------------------------------

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
  ;; run a complete game from initial moves thru final stages for the given external players
  ;; the list is sorted in ascending order of age; produce list of ranked players and cheaters
  ;; EFFECT provide observers with updates on regular turns
 (contract-out
  [referee actual-referee/c]))

;
;       ;                                  ;
;       ;                                  ;                          ;
;       ;                                  ;
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ;
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ;
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ;
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;
;                 ;
;                 ;
;                 ;

(require "../../util/measure.rkt")
(require "../common/referee-interface.rkt")
(require "../common/internal-player.rkt")
(require "../common/rules.rkt")
(require "../common/game-state.rkt")
(require "../lib/list.rkt")
(require "../lib/xsend.rkt")
(require "../common/referee-interface.rkt")

;
;       ;
;       ;           ;
;       ;           ;
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;
;   ;; ;;      ;    ;        ;          ;;  ; ;;  ;  ;; ;;
;   ;   ;      ;    ;        ;          ;     ;   ;; ;   ;
;   ;   ;   ;;;;    ;     ;;;;          ;     ;;;;;; ;   ;
;   ;   ;  ;   ;    ;    ;   ;          ;     ;      ;   ;
;   ;; ;;  ;   ;    ;    ;   ;          ;     ;      ;; ;;   ;;
;    ;;;;   ;;;;    ;;;   ;;;;          ;      ;;;;  ;;;;    ;;
;                                                    ;
;                                                    ;
;                                                    ;

#; {type Tiles* = [Listof TileIndex]}

#; {type Rankings = [Listof Player*]}
#; (type Player*  = [Listof Player])
#; {type Observer* = [Listof Observer]}

(define DEFAULT-ROWS 2)
(define DEFAULT-COLUMNS 9)

;
;                    ;;
;                   ;
;                   ;
;    ;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;    ;;;
;    ;;  ; ;;  ;    ;    ;;  ;   ;;  ; ;;  ;  ;;  ;
;    ;     ;   ;;   ;    ;   ;;  ;     ;   ;; ;   ;;
;    ;     ;;;;;;   ;    ;;;;;;  ;     ;;;;;; ;;;;;;
;    ;     ;        ;    ;       ;     ;      ;
;    ;     ;        ;    ;       ;     ;      ;
;    ;      ;;;;    ;     ;;;;   ;      ;;;;   ;;;;
;
;
;

(define (referee pre-state
                 #:time-out (time-out (time-out-limit))
                 #:observers (o*0 '())
                 #:lop   (lop '[])
                 #:size  (r-w #false )
                 #:fixed (fix #false))

  (time-out-limit time-out)

  (define state0 (if (cons? lop) (prepare-state lop r-w fix) pre-state))

  (define external* (map iplayer-payload (fishes-players state0)))

  (match-define (list post-inform cheaters0) (inform-about-self-and-others state0 external*))
  (define external*1 (remove* cheaters0 external*))

  (match-define (list post-placement cheaters1) (initial-placements post-inform external*1))
  (define external*2 (remove* cheaters1 external*1))

  (define o* (map (λ (uninit-observer) (uninit-observer post-placement)) o*0))

  (set! first-round #true)
  (match-define (list post-game cheaters2) (play-game (generate-tree post-placement) external*2 o*))
  (define ranked (rank-players (fishes-players post-game) external*2))

  (list ranked (append cheaters2 cheaters1 cheaters0)))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof Player] [Maybe [List N N]] [Maybe N] -> State}
(define (prepare-state lop r-w fix)
  (match-define (list row column) (or r-w (list DEFAULT-ROWS DEFAULT-COLUMNS)))
  (if fix
      (create-state row column lop #:fixed fix)
      (create-state row column lop)))

;; ---------------------------------------------------------------------------------------------------
#; {Player* Player* -> Rankings}
(define (rank-players players player*)
  (define sorted  (sort players > #:key iplayer-score))
  (define ranking (group-by iplayer-score sorted =))
  (map (λ (group) (map iplayer-payload group)) ranking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-modifiable
  #:level base
  (define actual-referee/c referee/c)

  #:level noop
  (define actual-referee/c referee-no-load/c)

  #:level check
  (define actual-referee/c referee-load/c))

;
;                    ;;
;      ;            ;
;                   ;
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;
;      ;   ;;  ;    ;    ;; ;;   ;;  ;;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;
;      ;   ;   ;    ;    ;; ;;   ;    ;  ;  ;
;    ;;;;; ;   ;    ;     ;;;    ;    ;  ;  ;
;
;
;

#; {State Player* -> (list State Player*)}
;; EFFECT tell each player about itself (color) and the other players (colors);
;; return state that represents surviving players
(define (inform-about-self-and-others state0 __external*)
  (define players (fishes-players state0))
  (define all-avatars (map iplayer-color players))

  (for/fold ([state state0] [cheats '()] #:result (list state (reverse cheats))) ([ip players])
    (define avatar (iplayer-color ip))
    (define external (iplayer-payload ip))
    (define void-failed (xsend external playing-as  #:caller "referee.rkt" avatar))
    (cond
      [(failed? void-failed)
       (values (delete-player state avatar) (cons external cheats))]
      [else
       (define void-failed
         (xsend external playing-with #:caller "referee.rkt" (remove avatar all-avatars)))
       (cond
         [(failed? void-failed)
          (values (delete-player state avatar) (cons external cheats))]
         [else
          (values state cheats)])])))

;
;
;      ;             ;     ;       ;          ;;;
;                          ;                    ;
;    ;;;   ; ;;    ;;;   ;;;;;   ;;;   ;;;;     ;
;      ;   ;;  ;     ;     ;       ;       ;    ;
;      ;   ;   ;     ;     ;       ;       ;    ;
;      ;   ;   ;     ;     ;       ;    ;;;;    ;
;      ;   ;   ;     ;     ;       ;   ;   ;    ;
;      ;   ;   ;     ;     ;       ;   ;   ;    ;
;    ;;;;; ;   ;   ;;;;;   ;;;   ;;;;;  ;;;;     ;;
;
;
;

#; {State Player* -> (list State Player*)}
;; every player gets to place one penguin at a time, until all players have placed all their penguins
(define (initial-placements state0 player*)

  (define penguin# (assigned-penguins player*))
  ;; play the entire init phase:
  (for/fold ([o-state state0] [o-cheats '()] #:result `(,o-state ,o-cheats)) ([_ penguin#])
    ;; play one init round:
    (define still-active (remove* o-cheats player*))
    (for/fold ([i-state o-state] [i-cheats o-cheats]) ([player still-active])
      (define-values (state cheated?) (one-initial-turn i-state))
      (values state (if cheated? (cons player i-cheats) i-cheats)))))

;; ---------------------------------------------------------------------------------------------------
#; {State -> (values State Boolean)}
;; ask `i` to pick the next posn for a penguin to produce a new state, with player removed if cheating
;; EFFECT MASK a failure to respond or an illegal placement gets `i` moved to `cheats`
;; ASSUME color of `i` is color of first player in `state`
(define (one-initial-turn state)
  (define active (first (fishes-players state)))
  (define avatar (iplayer-color active))
  (define external (iplayer-payload active))
  (define choice-failed (xsend external initial #:caller "referee.rkt" state))

  (define-values (state+ cheated?)
    (cond
      [(failed? choice-failed)
       (values (delete-player state avatar) #true)]
      [(legal-initial state choice-failed) => (λ (next) (values next #false))]
      [else
       (values (delete-player state avatar) #true)]))

  (values (next-player state+) cheated?))

;; ---------------------------------------------------------------------------------------------------
#; {State Posn -> (U False State)}
;; check the legality of the desired placement action for the active player,
;; construct resulting state including the rotation of player
;; ASSUME the first player is the one that places an avatar
(define (legal-initial state p)
  (define players (fishes-players state))
  (define places  (append-map iplayer-places players))
  (cond
    [(or (free-at state p) (member p places)) #false]
    [else (place-avatar state p)]))

;
;
;
;
;    ;;;;  ;;;;  ;;;;;;   ;;;
;   ;;  ;      ; ;  ;  ; ;;  ;
;   ;   ;      ; ;  ;  ; ;   ;;
;   ;   ;   ;;;; ;  ;  ; ;;;;;;
;   ;   ;  ;   ; ;  ;  ; ;
;   ;; ;;  ;   ; ;  ;  ; ;
;    ;;;;   ;;;; ;  ;  ;  ;;;;
;       ;
;    ;  ;
;     ;;

(define first-round #true)

#; {Tree Player* Observer* -> (list Rankings Player*)}
;; compute the outcome of a game, starting from the post-setup state
(define (play-game tree0 player*0 (o*0 '()))
  (set! first-round #true)
  (let play ([tree tree0] [o* o*0] [player* player*0] [actions '()] [cheats '()])
    ;; for time management and enbugging
    (when (>= (length actions) (length player*)) (set! first-round #false))
    (cond
      [(final? tree) (list (tree-current tree) cheats)]
      [else
       (tree-shape-assertion tree0 (reverse actions) tree) ;; for safety
       (define-values (t+ step o+) (play-turn tree (actions-since-last-turn actions player*) o*))
       (cond
         [step (play t+ o+ (rotate player*) (cons step actions) cheats)]
         [else (set! tree0 t+)
               (play t+ o+ (rest player*)   '[]                 (cons (first player*) cheats))])])))

#; {[Listof X] [Listof Y] -> [Listof X]}
(define (actions-since-last-turn actions0 player*)
  (if (<= (length actions0) (length player*)) '[] (reverse (take actions0 (- (length player*) 1)))))

#; {Tree [Listof Action] Tree -> Void}
;; does following the steps in `actions-from-tree0-to-tree` reach 'tree' from `tree0`?
(define (tree-shape-assertion tree0 actions-from-tree0-to-tree tree)
  (when tree0
    (define current-state  (tree-current tree))
    (define computed       (apply tree-path tree0 actions-from-tree0-to-tree))
    (define computed-state (tree-current computed))
    (unless (equal? computed-state current-state)
      (printf "comp: ~a\n" computed)
      (printf "actl: ~a\n" tree)
      (let-values ([(x _) (render-state computed-state)])
        (pretty-print `[computed state ,x]))
      (let-values ([(x _) (render-state current-state)])
        (pretty-print `[current state ,x]))
      (error 'tree-shape-assertion "assertion failure\n"))))

;
;
;     ;
;     ;
;   ;;;;;  ;   ;   ;;;;  ; ;;
;     ;    ;   ;   ;;  ; ;;  ;
;     ;    ;   ;   ;     ;   ;
;     ;    ;   ;   ;     ;   ;
;     ;    ;   ;   ;     ;   ;
;     ;    ;   ;   ;     ;   ;
;     ;;;   ;;;;   ;     ;   ;
;
;
;

#; {Tree [Listof Action] Observer* -> (values Tree (U Action False) Observer*)}
;; the active player takes a turn to get next tree, cheater (if), and surviving observers
;; if legal, take a step in tree
;; if not, generate new tree after deleting the (first) player
(define (play-turn tree actions-taken-since-last (observers '()))
  (parameterize ([time-out-limit (if first-round (ready-time) (time-out-limit))])
    (cond
      [(noop? tree) (define a (noop tree)) (values (take-action tree a) a observers)]
      [else (play-proper-turn tree actions-taken-since-last observers)])))

#; {Tree [Listof Action] Observer* -> (values Tree (U Action False) Observer*)}
;; the active player is now guaranteed to be able to play properly;
;; protect against failure and cheaters only
(define (play-proper-turn tree actions-taken-since-last observers)
  (define state (tree-current tree))
  (define active   (if (and (not no-bug?) (not first-round))
                       (last (fishes-players state))
                       (first (fishes-players state))))
  (define external (iplayer-payload active))
  (define avatar   (iplayer-color active))

  (define choice-failed
    (parameterize ([time-out-limit (time-out-limit)])
      (xsend external take-turn #:caller "referee.rkt" state actions-taken-since-last)))

  (define-values (msg state+ act)
    (cond
      [(failed? choice-failed)
       (define msg (~a avatar " failed: " (failed-value choice-failed)))
       (values msg (generate-tree (delete-player state avatar)) #false)]

      [(take-action tree choice-failed)
       => (λ (t) (values (tree-current t) t choice-failed))]

      [else
       (define from (first choice-failed))
       (define to   (second choice-failed))
       (define msg  (~a avatar " attempted to cheat, trying to move from " from " to " to))
       (values msg (generate-tree (delete-player state avatar)) #false)]))

  (define observers+  (xinform-observers observers state (if (string? msg) msg (list act msg))))
  (values state+ act observers+))

;
;          ;
;          ;
;          ;
;    ;;;   ;;;;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;
;   ;; ;;  ;; ;;  ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ; ;   ;
;   ;   ;  ;   ;  ;      ;   ;;  ;      ; ;   ;   ;;  ;     ;
;   ;   ;  ;   ;   ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;      ;;;
;   ;   ;  ;   ;      ;  ;       ;      ; ;   ;       ;         ;
;   ;; ;;  ;; ;;  ;   ;  ;       ;       ;    ;       ;     ;   ;
;    ;;;   ;;;;    ;;;    ;;;;   ;       ;     ;;;;   ;      ;;;
;
;
;

#; {[Listof Observer] (U [List Action State] String) -> [Listof Observer]}
;; inform observers about the current turn; produce all those that interact properly
(define (xinform-observers observers0 state turn-legal)
  (let loop ([observers observers0][broken '[]])
    (cond
      [(empty? observers) (remove* broken observers0)]
      [else
       (define o1 (first observers))
       (define void-failed (xcall o1 state turn-legal))
       (if (failed? void-failed)
           (loop (remove o1 (rest observers)) (cons o1 broken))
           (loop (rest observers) broken))])))
