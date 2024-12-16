#lang racket

;; represents tiles with fish on them
;; determine some basic physical parameters of the visuals

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

(require (only-in pict pict? standard-fish pict-height))

(define FISH-COLOR "blue")
(define TILE-COLOR "orange")

(define TILE-SIZE 30)
(define FISH      (standard-fish TILE-SIZE (/ TILE-SIZE 5) #:color FISH-COLOR))
(define DY 5)

(define MAX-FISH  (quotient (* 2 TILE-SIZE) (+ DY (pict-height FISH))))
(define fish#/c   (and/c natural-number/c (>=/c 1) (<=/c MAX-FISH)))

(provide
 TILE-SIZE ;; the length of one step in the visual hexagon
 MAX-FISH  ;; the maximum number of fish that can be stacked if their distance is DY

 (contract-out
  (fish#/c contract?)
  #; {type FishTile = (fish-tile n)}
  (fish-tile (-> fish#/c fish-tile?))
  (fish-tile-n (-> fish-tile? fish#/c))
  (tile-with-fish
   (->i ([n-fish fish-tile?])
        (#:coordinates (c [list/c natural-number/c natural-number/c]))
        (r pict?)))
  (empty-tile (-> string? pict?))))

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

(require "../lib/pict-hexagon.rkt")
(require (except-in pict pict? standard-fish pict-height))

;
;
;     ;        ;            ;                          ;     ;;;
;     ;                     ;                 ;                ;
;     ;                     ;                 ;                ;
;   ;;;;;    ;;;     ;;;;   ; ;;;           ;;;;;;   ;;;       ;     ;;;;    ;;;;
;     ;        ;    ;    ;  ;;   ;            ;        ;       ;    ;    ;  ;    ;
;     ;        ;    ;       ;    ;            ;        ;       ;    ;;;;;;  ;
;     ;        ;     ;;;;   ;    ;            ;        ;       ;    ;        ;;;;
;     ;        ;         ;  ;    ;            ;        ;       ;    ;            ;
;     ;        ;    ;    ;  ;    ;            ;        ;       ;    ;;   ;  ;    ;
;     ;      ;;;;;   ;;;;   ;    ;             ;;;   ;;;;;      ;;;  ;;;;;   ;;;;
;
;
;
;

(struct fish-tile [n] #:prefab)

(define (tile-with-fish f #:coordinates (coordinates #false))
  (define n-fish (fish-tile-n f))
  (cc-superimpose (filled-hexagon TILE-SIZE #:color TILE-COLOR)
                  (if coordinates
                      (text (~a coordinates) 'roman 18)
                      (apply vc-append DY (make-list n-fish FISH)))))

(define (empty-tile background-color)
  (filled-hexagon TILE-SIZE #:color background-color))
