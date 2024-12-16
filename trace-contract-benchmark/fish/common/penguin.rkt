#lang racket

;; characters that eat fish

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

(require (only-in pict pict?))

(define penguin/c (list/c string? pict?))

(provide
 (contract-out
  (penguin-color/c contract?)
  (penguin/c       contract?)
  (penguin-colors  (listof string?))
  (penguins        (listof penguin/c))))

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

(require "../common/fish.rkt")
(require (except-in pict pict?))
(require pict/convert)
(require racket/runtime-path)

;
;
;                                              ;
;
;
;   ; ;;;    ;;;;   ; ;;;    ;;; ;  ;    ;   ;;;    ; ;;;    ;;;;
;   ;;  ;;  ;    ;  ;;   ;  ;;  ;;  ;    ;     ;    ;;   ;  ;    ;
;   ;    ;  ;;;;;;  ;    ;  ;    ;  ;    ;     ;    ;    ;  ;
;   ;    ;  ;       ;    ;  ;    ;  ;    ;     ;    ;    ;   ;;;;
;   ;    ;  ;       ;    ;  ;    ;  ;    ;     ;    ;    ;       ;
;   ;;  ;;  ;;   ;  ;    ;  ;;  ;;  ;   ;;     ;    ;    ;  ;    ;
;   ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;; ;   ;;;;;  ;    ;   ;;;;
;   ;                            ;
;   ;                        ;  ;;
;   ;                         ;;;
;

(define penguin-colors `["red" "white" "brown" "black"])

(define penguin-color/c (apply or/c penguin-colors))

(define-runtime-path penguin-place "../resources/penguin.png")
(define (penguin c)
  (let* ([.png (pict-convert (bitmap penguin-place))]
         [base (freeze (scale .png (/ (- (* 2 TILE-SIZE) 4) (pict-height .png))))]
         [back (filled-rectangle (pict-width base) (pict-height base) #:color c)]
         [pict base #;(cc-superimpose back base)])
    (define (bullet r c) (filled-ellipse r r #:color c))
    (pin-over pict 20 28 (bullet 15 c))))

(define penguins
  (for/list ([c penguin-colors])
    (list c (penguin c))))
