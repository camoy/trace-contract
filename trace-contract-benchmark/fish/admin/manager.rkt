#lang racket/gui

;; a tournament manager that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed or not (boolean)

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

(require (only-in "../common/referee-interface.rkt" admin-player/c))
(require "../lib/list.rkt")

(define player*/c [listof admin-player/c])
(define results/c (list/c player*/c player*/c))

(provide
 (contract-out
  (results/c contract?)
  [manager
   ;; produces a list consisting of the tournament winners and failures/cheaters
   (->i ([lop (and/c player*/c cons? distinct?)])
        (#:fixed [f (or/c #false natural?)]
         #:size (s (list/c natural? natural?))
         #:t-observer (obs% (or/c #false class?))
         #:time-out (t-o positive?))
        (r results/c))]))

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

(require (except-in "../common/referee-interface.rkt" admin-player/c))
(require "../admin/prepare-games.rkt")
(require "../admin/referee.rkt")
(require "../lib/xsend.rkt")

;
;
;
;
;  ;;;;;;  ;;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;;
;  ;  ;  ;     ;  ;;  ;      ;  ;;  ;  ;;  ;   ;;  ;
;  ;  ;  ;     ;  ;   ;      ;  ;   ;  ;   ;;  ;
;  ;  ;  ;  ;;;;  ;   ;   ;;;;  ;   ;  ;;;;;;  ;
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;   ;  ;       ;
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;; ;;  ;       ;
;  ;  ;  ;  ;;;;  ;   ;   ;;;;   ;;;;   ;;;;   ;
;                                   ;
;                                ;  ;
;                                 ;;

#; {type Player* = [Listof Player]}
#; {type Results = [List Player* Player*]}
#; {type Referee = [[Listof Player] -> [List [Listof [Listof Player]] [Listof Player]]]}
#; {type Action  = }

#; {Player* -> Results}
;; produce a pair of the wunners and the cheaters
(define (manager lop0 #:time-out (t-o 3) #:fixed [f #f] #:size (s (list 5 5)) #:t-observer (obs% #f))
  (define run-one-game
    (if f
        (λ (lop) (referee #f #:size s #:time-out t-o #:lop lop #:fixed f))
        (λ (lop) (referee #f #:size s #:time-out t-o #:lop lop))))

  (time-out-limit 3)

  (match-define (list starters cheaters0)
    (inform-all/not-cheaters (λ (p msg) (xsend p start-of-tournament msg)) lop0 '[] '[]))

  (define-values (ranks cheaters) (run-all-games starters run-one-game obs%))
  (define winners (if (empty? ranks) '[] (first ranks)))

  (define cheaters1   (append cheaters0 cheaters))
  (define live-losers (filter (λ (p) (not (or (member p winners) (member p cheaters1)))) lop0))

  (inform-all/not-cheaters (λ (p msg) (xsend p end-of-tournament msg)) winners live-losers cheaters1))

;; ---------------------------------------------------------------------------------------------------
#;{[Listof Player] Referee Observer -> (values [Listof Player] [Listof Player])}
;; generative recursion: terminates because either the number of players shrinks per round
;; or the tournament is forcibly stopped because the surviving winners all tie for first place
(define (run-all-games lop0 run-one-game obs%)
  (parameterize ([current-eventspace (make-eventspace)])

    (define-syntax-rule (to-obs! method arg)
      (when obs
        (define result (xsend obs method arg))
        (when (failed? result)
          (set! obs #false))
        (when (or (eq? 'method 'show-winners) (eq? 'method 'show-next-round))
          (sleep 10))))

    (define obs (and obs% (new obs% [players lop0])))
    (to-obs! show #true)
    ;; accumulators: previous-winners and cheats
    (let loop ([lop1 lop0] [previous-winners '()] [cheats '()])
      (define lop  (re-sort lop1 lop0))
      (define lop# (length lop))
      (cond
        ;; not enough for one game
        [(< lop# MIN-PLAYERS)
         ;; observer
         (values (list lop) cheats)]
        ;; just enough for one game
        [(<= lop# MAX-PLAYERS)
         (to-obs! show-next-round (list lop))
         (match-define [list ranked new-cheats] (run-one-game lop))
         (to-obs! show-winners (list (first ranked)))
         (values ranked (append new-cheats cheats))]
        [else ;; keep going with rounds of games
         (define games (prepare-games MIN-PLAYERS MAX-PLAYERS lop))
         (to-obs! show-next-round games)
         (match-define `[,winners0 ,new-cheats] (run-one-round-of-games games run-one-game))
         (to-obs! show-winners winners0)
         (define winners (apply append winners0))

         (if (equal? winners previous-winners)
             (values (list winners) (append new-cheats cheats))
             (loop winners winners #;lop# (append new-cheats cheats)))]))))

;; ---------------------------------------------------------------------------------------------------
#; {Player* Referee -> [List Player* Player*]}
(define (run-one-round-of-games games run-one-game)
  (define results  (map run-one-game games))
  (define winners
    (map first
         (filter-map
          (λ (r)
            (match-define [list ranked _] r)
            (match ranked
              ['[] #f]
              [_ ranked]))
          results)))
  (define cheaters (append-map second results))
  (list winners cheaters))

;; ---------------------------------------------------------------------------------------------------
#;{Player* Player* -> Player*}
;; sort list of winners according to lop0
(define (re-sort winners lop0)
  (filter (λ (x) (member x winners)) lop0))

;
;                    ;;
;      ;            ;                                       ;;;    ;;;
;                   ;                                         ;      ;
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;         ;;;;     ;      ;
;      ;   ;;  ;    ;    ;; ;;   ;;  ;;  ;  ;            ;    ;      ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;            ;    ;      ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;         ;;;;    ;      ;
;      ;   ;   ;    ;    ;   ;   ;    ;  ;  ;        ;   ;    ;      ;
;      ;   ;   ;    ;    ;; ;;   ;    ;  ;  ;        ;   ;    ;      ;
;    ;;;;; ;   ;    ;     ;;;    ;    ;  ;  ;         ;;;;     ;;     ;;
;
;
;

#; {Action [Listof Player] [Listof Player] [Listof Player] -> Results}
;; EFFECT inform winners and losers; move players that fail this message into cheaters
(define (inform-all/not-cheaters action winners0 losers0 cheaters0)
  (define-values (winners cheats1)
    (for/fold ([winners '()][cheats cheaters0]) ([p (in-list winners0)])
      (inform-one action p #true winners cheats)))
  ;; --- if `losers0` is empty, `losers` is empty
  (define-values (losers cheaters)
    (for/fold ([losers '()][cheats2 cheats1]) ([r losers0])
      (inform-one action r #false losers cheats2)))
  ;; ---
  (list (reverse winners) (reverse cheaters)))

#; {Action [Player Boole [Listof Player] [Listof Player] -> (values [Listof Player] [Listof Player])]}
(define (inform-one action p msg winners cheats)
  (define void-failed (action p msg))
  (if (failed? void-failed)
      (values winners (cons p cheats))
      (values (cons p winners) cheats)))
