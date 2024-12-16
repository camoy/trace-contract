#lang racket

;; a board representation for "Fish.com"

#; {type Board = [Listof [Listof [Option FishTile]]]}
;; CONSTRAINTS All inner lists are of the same length.
;; INTERPRETATION a Board is a rectangle of FishTiles and Falses (holes).
;; The outer list denotes the rows, the inner list are cells within a row.

#| A column of height 4 has this shape:
   _______
  / (0,0) \ _______
  \_______// (1,0) \
  / (2,0) \\_______/
  \_______// (3,0) \
           \_______/

 Its width is 1. The data representation of this column is

 [[ [0,0] ]
  [ [1,0] ]
  [ [2,0] ]
  [ [3,0] ] ]

 because it has four rows.
|#

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

(require (only-in "../common/fish.rkt" fish#/c))
(require (only-in pict pict?))

(define board/c  (listof (listof any/c)))
(define posn/c   (list/c natural-number/c natural-number/c)) ;; row x column
(define routes/c any/c)

(define MAX-ROW 25)
(define MIN-ROW 1)
(define MAX-COLUMN 25)
(define MIN-COLUMN 1)
(define row/c (and/c natural? (>=/c MIN-ROW) (<=/c MAX-ROW)))
(define column/c (and/c natural? (>=/c MIN-COLUMN) (<=/c MAX-COLUMN)))

(provide

 MAX-ROW    ;; maximum number of rows
 MAX-COLUMN ;; maximum number of columns

 #; {type Stepper = Board -> Posn || Board Posn -> (U False Posn)}
 #; {Board Stepper [N Posn -> (U False X)] #:reserved [Listof Posn] -> [Listof X]}

 board-lr-td
 board-traverse

 (contract-out
  (posn/c   contract?)
  (board/c  contract?)
  (row/c    contract?)
  (column/c contract?)

  (make-board
   ;; create a `rows x columns` board with at least `1fish` tiles that displaay exactly one fish
   ;; do not place tiles at the `posn`s specified via holes; #:fixed means all tiles have N fish
   (->i ([rows row/c][columns column/c])
        [#:+     (1fish (rows columns) (and/c natural? (<=/c (+ (quotient (* rows columns) 10) 1))))
         #:-     (holes [listof posn/c])
         #:fixed (same-number-of-fish-per-tile fish#/c)]
        (r board/c)))

  (add-row
   ;; add a row of holes xor tiles with fixed number fish on there
   (-> board/c natural? (or/c #false natural?) board/c))

  (board-rows (-> board/c natural?))
  (board-columns (-> board/c natural?))

  (fish-at
   ;; how many fish are on this board at the specified position
   (-> board/c posn/c (or/c 0 fish#/c)))

  (remove-tile
   ;; remove the tile at the specified position from this board; also return the number of fishes
   (->i ([b board/c] [p posn/c]) #:pre (b p) (fish-at b p) (values (_ natural?) [_ board/c])))

  (neighboring-tiles
   ;; all neighboring tiles of the specified posn on this board
   (-> board/c posn/c [listof posn/c]))

  (all-possible
   ;; all tiles accessible via a straight line on this board,
   ;; starting from the given posn;
   ;; reserved posns are blockers
   (->i ([b board/c][p posn/c]) (#:reserved [reserved [listof posn/c]]) [r routes/c]))

  (render-board
   ;; render this board
   (->i ([b board/c])
        (#:+ [penguin-places [listof [cons/c pict? [listof posn/c]]]]
         #:color (color string?)
         #:arrow (arrow (list/c posn/c posn/c)))
        (r pict?)))

  (logic-posn->pict-center (-> posn/c posn/c))

  (any-pict-posn->locic-center
   ;; convert Pict position to logical center point of the tile, if it is contained; #f otherwise
   (->i ([x-y posn/c] #:rows (rows natural-number/c) #:columns (columns natural-number/c))
        (values (posn (or/c #false posn/c)) (center (list/c natural-number/c natural-number/c)))))

  ;; type Posn = [List Natural Natural]
  (posn-row    (-> posn/c natural-number/c))
  (posn-column (-> posn/c natural-number/c))))

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

(require (except-in "../common/fish.rkt" fish#/c))
(require "../lib/hexagon.rkt")
(require "../lib/toint.rkt")
(require (except-in pict pict?))

;
;
;   ;                          ;
;   ;
;   ;
;   ; ;;;     ;;;    ;;;;    ;;;      ;;;    ;;;;
;   ;;  ;;   ;   ;  ;    ;     ;     ;   ;  ;    ;
;   ;    ;       ;  ;          ;    ;       ;
;   ;    ;   ;;;;;   ;;;;      ;    ;        ;;;;
;   ;    ;  ;    ;       ;     ;    ;            ;
;   ;;  ;;  ;   ;;  ;    ;     ;     ;   ;  ;    ;
;   ; ;;;    ;;; ;   ;;;;    ;;;;;    ;;;    ;;;;
;
;
;
;


(define posn-row first)
(define posn-column second)

(define (make-board rows# columns# #:+ (1fish0 0) #:- (holes '()) #:fixed (f #f))
  (define 1fish 1fish0)
  (define spots (* rows# columns#))
  (define ((populate r) c)
    (set! spots (- spots 1))
    (cond
      [f (set! 1fish 0) (fish-tile f)]
      [(member (list r c) holes)
       #false]
      [(<= spots 1fish)
       (set! 1fish (- 1fish 1))
       (fish-tile 1)]
      [else
       (define n (+ (random MAX-FISH) 1))
       (when (= n 1) (set! 1fish (- 1fish 1)))
       (fish-tile n)]))
  (define candidate (build-list rows# (λ (r) (build-list columns# (populate r)))))
  (if (<= 1fish 0) candidate (make-board rows# columns# #:+ 1fish0 #:- holes)))

(define (add-row board how-wide fish#-or-false)
  (define width   (board-columns board))
  (define new-row
    (list
     (build-list width (λ (i) (and fish#-or-false (< i how-wide) (fish-tile fish#-or-false))))))
  (append board new-row))

(define (board-rows b) (length b))
(define (board-columns b) (apply max (map length b)))

#; {Board N N -> (U Tile #f)}
(define (board-ref board row column)
  (and (good-position board row column) (list-ref (list-ref board row) column)))

#; {Board N N -> Boolean}
(define (good-position board row column)
  (and (<= 0 row (- (length board) 1))
       (<= 0 column (- (length (first board)) 1))))

(define (fish-at board p)
  (match-define (list row column) p)
  (define f (board-ref board row column))
  (if (boolean? f) 0 (fish-tile-n f)))

(define (remove-tile board p)
  (match-define (list row column) p)
  (define n (fish-tile-n (board-ref board row column)))
  (define vboard (map list->vector board))
  (vector-set! (list-ref vboard row) column #false)
  (values n (map vector->list vboard)))

;
;
;
;    ;;;;;    ;
;   ;;        ;
;   ;       ;;;;;;   ;;;;   ; ;;;   ; ;;;    ;;;;    ;;;;    ;;;;
;   ;;        ;     ;    ;  ;;  ;;  ;;  ;;  ;    ;   ;;  ;  ;    ;
;    ;;;;;    ;     ;;;;;;  ;    ;  ;    ;  ;;;;;;   ;      ;
;        ;    ;     ;       ;    ;  ;    ;  ;        ;       ;;;;
;             ;     ;       ;    ;  ;    ;  ;        ;           ;
;   ;    ;    ;     ;;   ;  ;;  ;;  ;;  ;;  ;;   ;   ;      ;    ;
;    ;;;;;     ;;;   ;;;;;  ; ;;;   ; ;;;    ;;;;;   ;       ;;;;
;                           ;       ;
;                           ;       ;
;                           ;       ;
;

#; {type Stepper = Board -> Posn || Board Posn -> (U False Posn)}

#; Stepper
(define board-lr-td
  (case-lambda
    [(board) '[0 0]]
    [(board spot)
     (define width  (- (board-columns board) 1))
     (define height (- (board-rows board) 1))
     (match-define (list row col) spot)
     (cond
       [(< col width)  (list row (+ col 1))]
       [(< row height) (list (+ row 1) 0)]
       [else           #false])]))

(define (board-traverse board0 in-which-order f #:reserved (reserved '[]))
  (define board (remove-all board0 reserved))
  (let search-for-unocupied-non-hole-tiles ([spot* '()][spot (in-which-order board)])
    (define fish# (fish-at board spot))
    (define spot**
      (cond
        [(and (> fish# 0) (f fish# spot)) => (λ (x) (cons x spot*))]
        [else spot*]))
    (cond
      [(in-which-order board spot) => (curry search-for-unocupied-non-hole-tiles spot**)]
      [else (reverse spot**)])))

;
;
;
;
;
;   ;;;;;;   ;;;;   ;    ;   ;;;;    ;;;;
;   ;  ;  ; ;;  ;;  ;;  ;;  ;    ;  ;    ;
;   ;  ;  ; ;    ;   ;  ;   ;;;;;;  ;
;   ;  ;  ; ;    ;   ;  ;   ;        ;;;;
;   ;  ;  ; ;    ;   ;;;;   ;            ;
;   ;  ;  ; ;;  ;;    ;;    ;;   ;  ;    ;
;   ;  ;  ;  ;;;;     ;;     ;;;;;   ;;;;
;
;
;
;

(struct routes [nw no ne se so sw] #:prefab)
#; {type Routes = [routes Route Route Route Route Route]}
;; maximal pathways in the six feasible directions
#; {type Route  = [Listof Posn]}
#; {type Posn   = [List N N]}
#; {type Dir    = [List Z Z]}

;; -- from -- even --- odd ---
#; [List Dir Dir]
(define nw '[[-1 -1] [-1  0]])
(define no '[[-2  0] [-2  0]])
(define ne '[[-1  0] [-1 +1]])
(define se '[[+1  0] [+1 +1]])
(define so '[[+2  0] [+2  0]])
(define sw '[[+1 -1] [+1  0]])

(define (neighboring-tiles board p)
  (match-define (list row column) p)
  (for*/list ([δ (list no ne se so sw nw)][q (in-value (step p δ))] #:when (> (fish-at board q) 0))
    q))

(define (all-possible board0 p #:reserved (reserved '[]))
  (match-define (routes nw no ne se so sw) (all-possible-routes board0 p #:reserved reserved))
  (append no ne se so sw nw))

#; {Board Posn #:reserved [Listof Posn] -> Routes}
(define (all-possible-routes board0 p #:reserved (reserved '[]))
  (define board (remove-all board0 reserved))
  (apply routes (map (λ (d) (trace board p d)) (list nw no ne se so sw))))

(provide remove-all)

#; {Board [Listof Posn] -> Routes}
(define (remove-all board0 r)
  (for/fold ([n 0] [board board0] #:result board) ([r r]) (remove-tile board r)))

#; {Board Posn Dir -> Route}
;; ASSUME the board has holes where the trace must stop
(define (trace board start0 direction)
  (let trace ([start start0])
    (define next (step start direction))
    (cond
      [(off-board? board next) '()]
      [else (cons next (trace next))])))

#; {Board Posn -> Boolean}
(define (off-board? board position)
  (not (apply board-ref board position)))

#; {Board Posn Dir -> Posn}
;; step in the specified `direction` from the given `position`,
;; regardless of whether the result is a posn on the board
(define (step position direction)
  (match-define (list r c) position)
  (match-define (list d e) (if (even? r) (first direction) (second direction)))
  (list (+ r d) (+ c e)))

;
;
;                                ;                     ;
;                                ;
;                                ;
;    ;;;;    ;;;;   ; ;;;    ;;; ;   ;;;;    ;;;;    ;;;    ; ;;;    ;;; ;
;    ;;  ;  ;    ;  ;;   ;  ;;  ;;  ;    ;   ;;  ;     ;    ;;   ;  ;;  ;;
;    ;      ;;;;;;  ;    ;  ;    ;  ;;;;;;   ;         ;    ;    ;  ;    ;
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ;
;    ;      ;       ;    ;  ;    ;  ;        ;         ;    ;    ;  ;    ;
;    ;      ;;   ;  ;    ;  ;;  ;;  ;;   ;   ;         ;    ;    ;  ;;  ;;
;    ;       ;;;;;  ;    ;   ;;; ;   ;;;;;   ;       ;;;;;  ;    ;   ;;; ;
;                                                                        ;
;                                                                    ;  ;;
;                                                                     ;;;
;

(define BACKGROUND "gray")

(define (render-board board #:color (color BACKGROUND) #:arrow (arrow #false) #:+ (positions '[]))
  (define width  (+ 4 (* TILE-SIZE 4 (length (first board))) TILE-SIZE))
  (define height (+ 4 (* TILE-SIZE (+ (length board) 1))))
  (let* ([pict (filled-rectangle #:color color width height)]
         [pict (add-fish-and-penguin pict board positions)]
         [pict (if (boolean? arrow) pict (apply add-line #:with-arrow #t pict (apply append arrow)))])
    pict))

#; {Pict Board [Listof Posn] -> Pict}
(define (add-fish-and-penguin pict0 board penguin-positions)
  (for/fold ([pict pict0]) ([row board][r (in-naturals)])
    (for/fold ([pic pict]) ([cell row] [c (in-naturals)])
      (define-values (x y) (tile-position r c))
      (define cell-pic (if cell (tile-with-fish cell) (empty-tile BACKGROUND)))
      (pin-over pic x y (add-penguin cell-pic (list r c) penguin-positions)))))

#; {[Pict Posn [Listof [Cons Pict [Listof Posn]]] -> Pict]}
(define (add-penguin cell-pic0 position penguin-positions)
  (for/fold ([cell-pic cell-pic0]) ([p penguin-positions])
    (match-define (cons penguin position*) p)
    (if (member position position*)
        (cc-superimpose cell-pic penguin)
        cell-pic)))

#;{Pict Natural Natural Natural Natural  -> Pict}
(define (add-line s row0 column0 row1 column1 #:color (c "red") #:with-arrow (with #false))
  (define from (λ (_ _1) (apply values (logic-posn->pict-center (list row0 column0)))))
  (define to   (λ (_ _1) (apply values (logic-posn->pict-center (list row1 column1)))))
  (if with
      (pin-arrow-line 10 s s from s to #:color c #:line-width 3)
      (pin-line s s from s to #:color c #:line-width 3)))

;; convert row-column pair to x-y coordinate pair (center of tile at [row,column])
(define (logic-posn->pict-center p)
  (match-define [list r c] p)
  (define-values (x y) (tile-position r c))
  (list (toint (+ 2 x (* 1.5 TILE-SIZE))) (toint (+ 2 y TILE-SIZE))))

#; {N N ->* N N}
;; convert row-column pair to x-y coordinate pair (top left of tile at [row,column])
(define (tile-position row column)
  (define y (+ 2 (* TILE-SIZE row)))
  (if (odd? row)
      (values (+ 2 (* 4 TILE-SIZE column) (* 2 TILE-SIZE)) y)
      (values (+ 2 (* 4 TILE-SIZE column))                 y)))

;
;
;    ;;;                       ;                                                               ;
;      ;
;      ;
;      ;     ;;;;    ;;; ;   ;;;      ;;;           ;    ;   ;;;;            ;;; ;  ;    ;   ;;;
;      ;    ;;  ;;  ;;  ;;     ;     ;   ;          ;;  ;;  ;    ;          ;;  ;;  ;    ;     ;
;      ;    ;    ;  ;    ;     ;    ;                ;  ;   ;               ;    ;  ;    ;     ;
;      ;    ;    ;  ;    ;     ;    ;                ;  ;    ;;;;           ;    ;  ;    ;     ;
;      ;    ;    ;  ;    ;     ;    ;                ;;;;        ;          ;    ;  ;    ;     ;
;      ;    ;;  ;;  ;;  ;;     ;     ;   ;            ;;    ;    ;          ;;  ;;  ;   ;;     ;
;       ;;;  ;;;;    ;;; ;   ;;;;;    ;;;             ;;     ;;;;            ;;; ;   ;;; ;   ;;;;;
;                        ;                                                       ;
;                    ;  ;;                                                   ;  ;;
;                     ;;;                                                     ;;;
;

(define (any-pict-posn->locic-center x-y #:rows rows #:columns columns)
  (match-define `[,x ,y] x-y)
  (define (in-hexagon? row column)
    (define-values (top-x top-y) (tile-position row column))
    (define local-x (- x top-x))
    (define local-y (- y top-y))
    (and (positive? local-x) (positive? local-y) (hexagon-within TILE-SIZE local-x local-y)))

  (define place
    (for*/first ([row (in-range rows)] [column (in-range columns)] #:when (in-hexagon? row column))
      (list row column)))

  (cond
    [(boolean? place) (values #false '[0 0])]
    [else
     (match-define (list cx cy) (logic-posn->pict-center place))
     (values place (list cx cy))]))
