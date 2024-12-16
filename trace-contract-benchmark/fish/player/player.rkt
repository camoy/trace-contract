#lang racket

;; this component implements the mechanics of the player

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

(define MIN-DEPTH 1)
(define MAX-DEPTH 5)
(define (good-depth d)
  (and (integer? d) (<= MIN-DEPTH d MAX-DEPTH)))

(provide
 MIN-DEPTH
 MAX-DEPTH

 good-depth
 (contract-out
  [make-player (-> string? (class/c) output-player/c)]))

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
(require "../player/strategy-interface.rkt")
(require (except-in "../common/rules.rkt"))
(require "../common/game-state.rkt")
(require "../common/internal-player.rkt")
(require "../common/penguin.rkt")
(require "../common/player-interface.rkt")

;
;   ;
;   ;
;   ;
;   ;;;;   ;;;;    ;;;    ;;;
;   ;; ;;      ;  ;   ;  ;;  ;
;   ;   ;      ;  ;      ;   ;;
;   ;   ;   ;;;;   ;;;   ;;;;;;
;   ;   ;  ;   ;      ;  ;
;   ;; ;;  ;   ;  ;   ;  ;
;   ;;;;    ;;;;   ;;;    ;;;;
;
;
;

;; internals of Player
;; internally, the player is game mechanics while the strategy component makes game decisions

(define base-player%
  (class object% (init-field strategy [name (gensym)])
    (field (me  (first penguin-colors)))
    (field (other-players '()))
    (field (tree #false))

    [define/public (playing-as my-name)
      (set! me my-name)]

    [define/public (playing-with others)
      (set! other-players others)]

    (define/public (initial state)
      (send strategy place-penguin state))

    [define/public (take-turn state actions-since-last-turn)
      (error 'take-turn "abstract")]

    (define/public (start-of-tournament nicknames)
      (void))

    [define/public (end-of-tournament results)
      (void)]

    (super-new)))

(define (make-player n strategy%)
  (new player% [name n] [strategy (new strategy%)]))

(define player%
  (class base-player%
    (inherit-field strategy me other-players tree)

    [define/override (take-turn state actions-since-last-turn)
      (set! tree (generate-tree state))
      ;; I could update the tree here but I'll just stay functional
      (send strategy move-penguin tree)]

    (super-new)))

(define imperative-player%
  (class base-player%
    (inherit-field strategy me other-players tree)

    [define/override (take-turn state actions-of-others-since-last-turn)
      (set! tree
            (if (empty? actions-of-others-since-last-turn)
                (generate-tree state)
                (apply tree-path tree actions-of-others-since-last-turn)))
      (define best-action (send strategy move-penguin tree))
      (set! tree (tree-path tree best-action))
      best-action]

    (super-new)))

;
;   ;                 ;
;   ;                 ;
;   ;                 ;
;   ;;;;   ;;;;    ;;;;   ;;;
;   ;; ;;      ;  ;; ;;  ;   ;
;   ;   ;      ;  ;   ;  ;
;   ;   ;   ;;;;  ;   ;   ;;;
;   ;   ;  ;   ;  ;   ;      ;
;   ;; ;;  ;   ;  ;; ;;  ;   ;
;   ;;;;    ;;;;   ;;;;   ;;;
;
;
;

;; players that misbehave w/ something that could be an action but isn't:

(define-syntax-rule (define/override-m % m) (define % (class player% (super-new) m)))

(define/override-m bad-init-choice%  ;; break contract or pick any position that's taken
  (define/override (initial state)
    (define players (fishes-players state))
    (for*/first ([p players] [penguins (in-value (iplayer-places p))] #:when (cons? penguins))
      (first penguins))))

(define/override-m bad-turn-choice% ;; chooses an action that takes the first penguin to itself
  (define/override (take-turn state actions)
    (define players (fishes-players state))
    (define my-penguins (iplayer-places (first players)))
    (list (first my-penguins) (first my-penguins))))

;; players that go into infinite loops:

(define-syntax-rule (define/time % m) (define/override-m % (define/override (m . x) (let L () (L)))))

(define/time bad-init-time% initial)

(define/time bad-turn-time% take-turn)

;; players that raise exceptions
(define-syntax-rule (define/raise % m n) (define/override-m % (define/override (m . x) (raise n))))

(define/raise bad-playing-as% playing-as 0)

(define/raise bad-playing-with% playing-with 1)

(define/raise bad-end-of-tournament% end-of-tournament 3)

(define/raise bad-start-of-tournament% end-of-tournament 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-modifiable
  #:level base
  (define output-player/c player/c)

  #:level noop
  (define output-player/c player-no-load/c)

  #:level check
  (define output-player/c player-load/c))
