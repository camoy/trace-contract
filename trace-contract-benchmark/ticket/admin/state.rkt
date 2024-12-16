#lang racket

;; representation of a referee's knowledge about the game

(require "../common/player-interface.rkt")
(require "../common/basic-constants.rkt")

(provide

 #; {RefereeState (U False [List MePlayer Boolean] [List MePlayer [Listof Cards]])
                  -> [Listf RefereeState Boolean]}
 if-rstate-update

 #; {RefereeState -> RefereeState}
 ;; ASSUME there is a first player
 rstate-rotate

 #; {RefereeState -> RefereeState}
 ;; ASSUME there is a first player
 rstate-drop
 rstate->pstate

 (contract-out
  (rstate (-> (listof ii?)
              (listof color?)
              (listof player/c)
              rstate?)))

 rstate-players
 rstate-cards
 rstate-drop-outs)

;
;
;        ;                                       ;                             ;
;        ;                                       ;                             ;
;        ;                                       ;
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;;   ;   ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ;
;   ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;          ;;;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ;
;    ;  ;;   ;      ;;  ;    ;      ;    ;   ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ;
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(require "../common/connection.rkt")
(require "../common/state.rkt")
(require "../common/map.rkt")

#;(module+ examples
  (provide rstate1 ii1 ii2))

#;(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod "../common/state.rkt" examples))
  (require (submod "../common/map.rkt" examples))
  (require rackunit))

;
;
;
;                     ;               ;
;                     ;               ;
;    ;;;;;   ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;
;    ;;     ;    ;    ;      ;   ;    ;      ;  ;;
;    ;      ;         ;          ;    ;     ;    ;
;    ;      ;;;       ;      ;;;;;    ;     ;;;;;;
;    ;         ;;;    ;     ;    ;    ;     ;
;    ;           ;    ;     ;    ;    ;     ;
;    ;      ;    ;    ;     ;   ;;    ;      ;
;    ;       ;;;;      ;;;   ;;; ;     ;;;   ;;;;;
;
;
;
;

(struct rstate [players cards drop-outs] #:transparent)
#; {type RefereeState = (rstate [Listof [MePlayer XPlayer]] [Listof Cards] [Listof XPlayer])}
#; {type XPlayer      = .. see player interface ..}

;; the MeState must be parameterized over a paylaod

#;(module+ examples
  (define cards1 (hash 'green 5))
  (define dest1  (list 'Boston 'Seattle))
  (define ii1 (ii dest1 '(Boston Orlando) 40 cards1 (set) #f))
  (define ii2 (ii dest1 '(Orlando Seattle) 5 cards1 (set [connection Boston Seattle red 3]) #f))

  (define rstate1 (rstate (list ii1 ii2) '[] '[]))
  (provide rstate1-drop)
  (define rstate1-drop (rstate (list ii2) '[] `[#f]))

  (provide rstate1-r rstate1-d)
  (define rstate1-r (rstate (list ii2 ii1) '[] '[]))
  (define rstate1-d (rstate (list ii2) '[] `[,ii1])))

;
;
;      ;;;                                     ;
;     ;                               ;        ;
;     ;                               ;
;   ;;;;;;  ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;
;     ;     ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ;
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;;
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ;
;     ;     ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ;
;     ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;
;
;
;
;

(define (rstate-drop the-state)
  (match-define [rstate (cons first others) cards drop-outs] the-state)
  (rstate others cards (cons (ii-payload first) drop-outs)))

#; {RefereeState -> RefereeState}
(define (rstate-rotate the-state)
  (match-define [rstate (cons first others) cards drop-outs] the-state)
  (rstate (append others (list first)) cards drop-outs))

#; {RefereeState -> PlayerState}
(define (rstate->pstate rs)
  (define players (map ii-payload-- (rstate-players rs)))
  (pstate (first players) (map ii-connections (rest players))))

(define (if-rstate-update the-state false-or-nup)
  (match false-or-nup
    [#false #false]
    [(list nup (? boolean? final?)) (list (rstate-update the-state nup) final?)]
    [(list nup cards) (list (rstate-update the-state nup cards) #false)]))

#; {RefereeState MePlayer [ [Listof Card] ] -> RefereeState}
;; HOW MANY CARDS should it take? 0 or CARDS-PER
(define (rstate-update rs nup (nu-cards '()))
  (match-define (rstate players cards drops) rs)
  (rstate (cons nup (rest players)) (rem2 cards nu-cards) drops))

#; {[Listof Color] [Listof Color] -> [Listof Color]}
(define (rem2 cards0 nu-cards0)
  (let rem2 [(cards cards0) (nu-cards nu-cards0)]
    (cond
      [(empty? nu-cards) cards]
      [(equal? (first cards) (first nu-cards)) (rem2 (rest cards) (rest nu-cards))]
      [else (error 'rem2 "can't happen" cards0 nu-cards0)])))



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

  (check-equal? (rstate-drop rstate1) rstate1-drop)
  (check-equal? (rstate-rotate rstate1) rstate1-r)

  (check-equal? (rstate->pstate rstate1) (pstate ii1 (list (ii-connections ii2)))))
