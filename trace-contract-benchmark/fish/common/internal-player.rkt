#lang racket

;; represents the "ground truth" state of players:
;; -- what the referee knows about the players,
;; -- what other players know about each other.

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

(require (only-in "../common/penguin.rkt" penguin/c))
(require (only-in "../common/fish.rkt" fish#/c))
(require (only-in "../common/board.rkt" posn/c))
(require (only-in pict pict?))

(provide
 (contract-out
  (iplayer? contract?)

  (create-player
   ;; set up an internal player representation, knowledge about players
   (-> penguin/c any/c iplayer?))

  (upscore-player
   ;; increase this player's running score
   (-> iplayer? fish#/c iplayer?))

  (+place-player
   ;; add an avatar location to this player's places
   (-> iplayer? posn/c iplayer?))

  (move-player
   ;; move an avatar of this player from old to new
   (->i ([p iplayer?] [old (p) posn/c] [new posn/c])
        #:pre/name (old p) "avatar exists" (member old (iplayer-places p))
        [r (old) (and/c iplayer? (λ (np) (not (member old (iplayer-places np)))))]))

  (iplayer-penguin (-> iplayer? pict?))
  (iplayer-color   (-> iplayer? string?))
  (iplayer-places  (-> iplayer? (listof posn/c)))
  (iplayer-payload (-> iplayer? any))
  (iplayer-score   (-> iplayer? natural-number/c))))

;
;
;        ;
;        ;            ;
;        ;            ;
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;    ;;;;   ; ;;;
;   ;;  ;;   ;   ;    ;      ;   ;           ;;  ;  ;    ;  ;;  ;;
;   ;    ;       ;    ;          ;           ;      ;;;;;;  ;    ;
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;       ;    ;
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;
;   ;;  ;;  ;   ;;    ;     ;   ;;           ;      ;;   ;  ;;  ;;    ;;
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;
;                                                           ;
;                                                           ;
;                                                           ;
;

(struct iplayer [color penguin score places payload] #:prefab)
#; {type InternalPlayer = (player ColorString Penguin Score [Listof Posn/c])}
;; the player is represented by Penguin in the visual display, uses the specified color, has collected
;; `score` fish, its penguins occupy the `places`, and an external client may use `payload` for ANY/C

(define (create-player pc x)
  (match-define (list color penguin) pc)
  (iplayer color penguin 0 '[] x))

(define (upscore-player p delta)
  (struct-copy iplayer p [score (+ (iplayer-score p) delta)]))

(define (+place-player p place)
  (struct-copy iplayer p [places (cons place (iplayer-places p))]))

(define (move-player p old nu)
  (struct-copy iplayer p [places (cons nu (remove old (iplayer-places p)))]))
