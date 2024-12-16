#lang racket

;; represent individual connections

;
;
;
;                                             ;
;                                             ;
;    ;;;;   ;;  ;;  ; ;;;    ;;;;    ;;;;   ;;;;;;   ;;;;
;   ;    ;   ;  ;   ;;  ;;  ;;  ;;   ;;  ;    ;     ;    ;
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;
;   ;         ;;    ;    ;  ;    ;   ;        ;      ;;;;
;   ;         ;;    ;    ;  ;    ;   ;        ;          ;
;   ;;   ;   ;  ;   ;;  ;;  ;;  ;;   ;        ;     ;    ;
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;
;                   ;
;                   ;
;                   ;
;

(provide
 ;; SYNTAX for data construction
 #;(_ a:id b:id c:id d)
 ;; with implicit quote for a b and c
 #;(_ a b c d)
 ;; evaluate all 4 arguments
 #;(_ from x)
 ;; from one city and list describing the rest of the connection
 #;(_ from-to x y)
 ;; from a 2-list of cities and a 2-list of color/seg#
 ;; the generated syntax calls a contract-protected constructor
 (rename-out [make-connection connection])
 (rename-out [connection/c action?])

 connection/c

 connection-from
 connection-to
 connection-ft ;; select the two cities as a list
 connection-color
 connection-seg#

 connection-flip ;; flip the direction

 connection-serialize
 parse-connection)

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

(require (only-in "basic-constants.rkt" city? color? seg#? list-cities))

(require syntax/parse/define)

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

(struct connection [from to color seg#] #:prefab)

(define/contract (checked-connection x y z w)
  (-> symbol? symbol? color? seg#? any)
  (connection x y z w))

(define-syntax-parser make-connection
  [(_ a:id b:id c:id d) #'(checked-connection 'a 'b 'c d)]
  [(_ a b c d)          #'(checked-connection a b c d)]
  [(_ from x)           #'(apply checked-connection from x)]
  [(_ from-to x y)      #'(let* ((c1-c2 from-to)
                                 (c1 (first c1-c2))
                                 (c2 (second c1-c2)))
                            (checked-connection c1 c2 x y))])

;
;
;     ;
;     ;
;     ;
;   ;;;;;   ;    ;  ; ;;;    ;;;;
;     ;     ;    ;  ;;   ;  ;    ;
;     ;     ;    ;  ;    ;  ;
;     ;     ;    ;  ;    ;   ;;;;
;     ;     ;    ;  ;    ;       ;    ;;
;     ;     ;   ;;  ;    ;  ;    ;    ;;
;     ;      ;;; ;  ;    ;   ;;;;     ;;
;
;
;
;

(define (connection-flip x)
  (connection (connection-to x) (connection-from x) (connection-color x) (connection-seg# x)))

(define (connection-ft c)
  (list (connection-from c) (connection-to c)))

(define (connection-good? x)
  (symbol<? (connection-from x) (connection-to x)))

(define connection/c (and/c connection? connection-good?))

;
;
;                              ;             ;;;       ;
;                                              ;
;                                              ;
;    ;;;;    ;;;;    ;;;;    ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;
;   ;    ;  ;    ;   ;;  ;     ;     ;   ;     ;       ;        ;;  ;    ;
;   ;       ;;;;;;   ;         ;         ;     ;       ;       ;;   ;;;;;;
;    ;;;;   ;        ;         ;     ;;;;;     ;       ;      ;;    ;
;        ;  ;        ;         ;    ;    ;     ;       ;     ;;     ;
;   ;    ;  ;;   ;   ;         ;    ;   ;;     ;       ;    ;;      ;;   ;
;    ;;;;    ;;;;;   ;       ;;;;;   ;;; ;      ;;;  ;;;;;  ;;;;;;   ;;;;;
;
;
;
;

(define (connection-serialize c)
  (match-define [connection city1 city2 color seg#] c)
  (append (map ~a (list-cities city1 city2)) (list (~a color) seg#)))


(define (parse-connection x #:check (check values))
  (check
   (match x
     [(list (? city? city1) (? city? city2) (? color? c) (? seg#? s))
      (connection (string->symbol city1) (string->symbol city2) (string->symbol c) s)]
     [_ #false])))
