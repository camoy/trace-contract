#lang racket

;; implement a simple greedy strategy:
;; -- place penguins on a tile with maximal fish number
;; -- move he penguin to a tile with maximal fish number
;;;   using the `tiebreaker` if there are several


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
  greedy-strategy
  (rename-out [greedy-strategy strategy%]))

;
;
;        ;                                       ;                             ;
;        ;                                       ;
;        ;                                       ;
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;
;   ;;  ;;  ;    ;  ;;  ;;  ;    ;  ;;   ;  ;;  ;;  ;    ;  ;;   ;   ;   ;     ;    ;    ;  ;    ;
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;        ;;;;
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ;
;   ;;  ;;  ;;   ;  ;;  ;;  ;;   ;  ;    ;  ;;  ;;  ;;   ;  ;    ;   ;   ;     ;    ;;   ;  ;    ;
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;    ;;;;;   ;;;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(require "../player/strategy-interface.rkt")
(require (except-in "../common/rules.rkt" tree?))
(require (except-in "../common/game-state.rkt" fishes? turn? move/c))
(require (except-in "../common/board.rkt" posn/c))

;
;                                   ;
;                                   ;
;                                   ;
;    ;;;;   ;;;;   ;;;    ;;;    ;;;;  ;   ;
;   ;;  ;   ;;  ; ;;  ;  ;;  ;  ;; ;;  ;   ;
;   ;   ;   ;     ;   ;; ;   ;; ;   ;   ; ;
;   ;   ;   ;     ;;;;;; ;;;;;; ;   ;   ; ;
;   ;   ;   ;     ;      ;      ;   ;   ; ;
;   ;; ;;   ;     ;      ;      ;; ;;   ;;
;    ;;;;   ;      ;;;;   ;;;;   ;;;;    ;
;       ;                                ;
;    ;  ;                               ;
;     ;;                               ;;

(define greedy-strategy
  (class base-strategy%
    (super-new)

    (define/override (evaluate trn state)
      (fish-at (fishes-board (tree-current state)) (second trn)))

    (define/augment (choose _the-max _xvalue)
      '[])))
