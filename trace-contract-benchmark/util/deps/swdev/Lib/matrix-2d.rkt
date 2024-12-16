#lang racket

(define (same-length? M) (apply = (map length M)))
(define rectangle? (and/c (listof (listof any/c)) cons? same-length?))
(define (row/c m) (flat-named-contract "row index" (and/c natural? (</c (matrix-#rows m)))))
(define (col/c m) (flat-named-contract "column index" (and/c natural? (</c (matrix-#columns m)))))

(provide
 matrix?
 direction?
 
 rectangle?
 rectangle-#rows
 rectangle-#columns
 
 (contract-out
  [matrix           (->* () #:rest rectangle? matrix?)]
  [make-matrix      (-> rectangle? matrix?)]

  [matrix-undo      (-> matrix? any)]
  [matrix-copy      (-> matrix? matrix?)]
  
  [matrix-#rows     (-> matrix? natural?)]
  [matrix-#columns  (-> matrix? natural?)]
  [matrix-transpose (-> matrix? matrix?)]
  [matrix-ref       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)]) (x any/c))]
  [matrix-set       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)] [n any/c]) (y any/c))]
  
  [matrix-rectangle (-> matrix? (listof (listof any/c)))]
  
  [matrix-pad
   (->i ([m matrix?] [lox [listof any/c]] #:nuwidth (nuw natural?) #:nuheight (nuh natural?))
        #:pre/name (m nuw) "new width >= to old width expected" (>= nuw (matrix-#columns m))
        #:pre/name (m nuh) "new height >= to old height expected" (>= nuh (matrix-#rows m))
        #:pre/name (m nuh nuw lox) "sufficient number of extras expected"
        (>= (length lox) (* (- (matrix-#columns m) nuw) (- (matrix-#rows m) nuh)))
        (r matrix?)
        #:post/name (nuw r) "expected width" (= (matrix-#columns r) nuw)
        #:post/name (nuh r) "expected height" (= (matrix-#rows r) nuh))]

  [matrix-slide-row    (->i ([m matrix?] [d left-right/c] [r (m) (row/c m)] [x any/c]) (y matrix+))]
  [matrix-slide-column (->i ([m matrix?] [d up-down/c] [c (m) (col/c m)] [x any/c]) (y matrix+))]  

  [matrix-left      direction?]
  [matrix-right     direction?]
  [matrix-up        direction?]
  [matrix-down      direction?]))

(module+ test
  (require (submod ".."))
  (require rackunit))

;                                                                          
;                                                                          
;        ;                                       ;            ;            
;        ;            ;                          ;            ;            
;        ;            ;                          ;            ;            
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;; ;   ;;;;   ;;;;;          
;   ;;  ;;   ;   ;    ;      ;   ;          ;;  ;;  ;    ;    ;            
;   ;    ;       ;    ;          ;          ;    ;  ;;;;;;    ;            
;   ;    ;   ;;;;;    ;      ;;;;;          ;    ;  ;         ;            
;   ;    ;  ;    ;    ;     ;    ;          ;    ;  ;         ;       ;;   
;   ;;  ;;  ;   ;;    ;     ;   ;;          ;;  ;;  ;;   ;    ;       ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;;; ;   ;;;;;    ;       ;;   
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(struct inner [rectangle {undo #:mutable} row# col#] #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc self other rec) (rec (inner-rectangle self) (inner-rectangle other)))
   (define (hash-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))
   (define (hash2-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))])
#; {type Inner = [inner [Vectorof [Vectorof X]] (U False (-> Matrix)) N N]}
;; undo can reverse the last imperative action performed on inner's rectangle 

(define matrix? inner?)
(define matrix+ (list/c matrix? any/c))

[define (matrix . t-rows)
  (define x (list->vector (map list->vector t-rows)))
  (inner x #false (vector-length x) (vector-length (vector-ref x 0)))]

[define (make-matrix t-rows)
  (apply matrix t-rows)]
  
(define matrix-#rows inner-row#)

(define matrix-#columns inner-col#)

(define (matrix-undo M)
  (define U (inner-undo M))
  (when U [U]))

(define (matrix-ref M r c)
  (vector-ref (vector-ref (inner-rectangle M) r) c))

(define (matrix-set M r c x)
  (define R (inner-rectangle M))
  (define y (vector-ref (vector-ref R r) c))
  (vector-set! (vector-ref R r) c x)
  (set-inner-undo! M (λ () (matrix-set M r c y)))
  M)

(define (matrix-ref- M r c) (matrix-ref M c r))

(define (matrix-set- M r c x) (matrix-set M c r x))

#; {∀ X: [Rectangle X] -> Natural}
(define (rectangle-#rows x) (length x))

(define (rectangle-#columns x) (length (first x)))

;                                                                                  
;                                                                                  
;        ;     ;                                       ;                           
;        ;                                    ;                                    
;        ;                                    ;                                    
;    ;;; ;   ;;;     ;;;;    ;;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;  
;   ;;  ;;     ;     ;;  ;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ; 
;   ;    ;     ;     ;      ;;;;;;  ;         ;        ;    ;    ;  ;    ;  ;      
;   ;    ;     ;     ;      ;       ;         ;        ;    ;    ;  ;    ;   ;;;;  
;   ;    ;     ;     ;      ;       ;         ;        ;    ;    ;  ;    ;       ; 
;   ;;  ;;     ;     ;      ;;   ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ; 
;    ;;; ;   ;;;;;   ;       ;;;;;    ;;;      ;;;   ;;;;;   ;;;;   ;    ;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


(struct direction [gen] #:property prop:procedure 0)

(define matrix-left  (direction (λ (w) (values w (curry - w) 0 w matrix-right))))
(define matrix-right (direction (λ (w) (values 0 identity    w 0 matrix-left))))
(define matrix-up    [direction (λ (h) (values h (curry - h) 0 h matrix-down))])
(define matrix-down  [direction (λ (h) (values 0 identity    h 0 matrix-up))])

#; {type Direction = [direction [(Listof X) -> X] [X [Listof X] -> [Listof X]]]}

(define left-right/c
  (flat-named-contract "left-right" (or/c (curry eq? matrix-left) (curry eq? matrix-right))))

(define up-down/c
  (flat-named-contract "up-down" (or/c (curry eq? matrix-down) (curry eq? matrix-up))))

;; ---------------------------------------------------------------------------------------------------
;; recover the given list-based rectangle
(define (matrix-rectangle M)
  (define R (inner-rectangle M))
  (for/list ([r R]) (vector->list r)))

;; ---------------------------------------------------------------------------------------------------
;; create the transpose of a matrix 

(define (matrix-transpose M)
  (match-define [inner R _ r c] M)
  (define R^T
    (for/vector ([c (in-range 0 c)])
      (for/vector ([r (in-range 0 r)])
        (vector-ref (vector-ref R r) c))))
  (inner R^T #false c r))

(define (matrix-copy M)
  (match-define [inner R _ r c] M)
  (define C
    (for/vector ([r (in-range 0 r)])
      (for/vector ([c (in-range 0 c)])
        (vector-ref (vector-ref R r) c))))
  (inner C #false r c))

;; ---------------------------------------------------------------------------------------------------
;; pad a matrix

(define (matrix-pad M extras #:nuwidth (nuw 7) #:nuheight (nuh 7))
  (make-matrix (rectangle-col-fill (matrix-rectangle M) extras nuw nuh)))

#; {∀ X: [Rectable X] [Listof X] N N -> [Rectangle X]}
;; padding a rectangle
(define (rectangle-col-fill R extras nuw nuh)
  (let rectangle-col-fill ([R R] [extras extras] [result '()])
    (cond
      [(empty? R) (rectangle-row-fill (reverse result) extras nuw nuh)]
      [else
       (define row (first R))
       (define nnn (- nuw (length row)))
       (define pad (append row (take extras nnn)))
       (rectangle-col-fill (rest R) (drop extras nnn) (cons pad result))])))

#; {∀ X: [Rectable X] [Listof X] N N -> [Rectangle X]}
(define (rectangle-row-fill R extras nuw nuh)
  (define height (length R))
  (let rectangle-row-fill ([todo (- nuh height)] [extras extras] [result (reverse R)])
    (cond
      [(or (zero? todo) (empty? extras)) (reverse result)]
      [else
       (define next-row (take extras nuw))
       (rectangle-row-fill (sub1 todo) (drop extras nuw) (cons next-row result))])))

;; ---------------------------------------------------------------------------------------------------
;; sliding a row or column 

#; {type ∀X: [Operate X] = {{Matrix X} Directon N X -> {Matrix X}}}
#; {∀X: [[Matrix X] N N -> Any] [[Matrix X] N N X -> Matrix] [[X Matrix] -> N] -> [Operate X]}
[define (make-workhorse ref set inner-dim)

  #; [Operate X] 
  ;; EFFECT set undo to the opposite action 
  (define (workhorse M dir n X)
    (define-values (Y opp) (shift M dir n X))
    (set-inner-undo! M (λ () (workhorse M opp n Y)))
    (list M Y))

  #; {[Matrix X] Direction N X -> (values X Direction)}
  ;; EFFECT shirft row/column `n` of `M` in direction `dir`. insert `X` into free spot
  (define (shift M dir n X)
    (define N (- (inner-dim M) 1))
    (define-values [saved0 step first last opp] [dir N])
    (define Y (ref M n first))
    (for/fold ([saved (ref M n saved0)]) ([i (in-range N)])
      (let ([i (step (+ i 1))])
        (begin0 (ref M n i) (set M n i saved))))
    (set M n last X)
    (values Y opp))
  
  workhorse]

(define matrix-slide-column (make-workhorse matrix-ref- matrix-set- inner-row#))

(define matrix-slide-row (make-workhorse matrix-ref matrix-set inner-col#))

;                                     
;                                     
;    ;                    ;           
;    ;                    ;           
;  ;;;;;    ;;;    ;;;; ;;;;;    ;;;; 
;    ;     ;   ;  ;       ;     ;     
;    ;     ;;;;;  ;;;     ;     ;;;   
;    ;     ;         ;;   ;        ;; 
;    ;     ;          ;   ;         ; 
;    ;;;    ;;;;  ;;;;    ;;;   ;;;;  
;                                     
;                                     
;                                     

(module+ test
  (define R
    '[[A C E]
      [B D F]])

  (define M0 (apply matrix R))

  (define M1
    (matrix
     '[A C E]
     '[B D F]))

  (define M1-at-3-x-4
    (matrix
     '[A C E a]
     '[B D F b]
     '[c d e f]))

  (define M1-make
    (make-matrix
     '[[A C E]
       [B D F]]))

  (define M1-set-1-1-X
    (matrix
     '[A C E]
     '[B X F]))

  (define M1-slide-row1-left-X
    (list
     (matrix
      '[A C E]
      '[D F X])
     'B))

  (define M1-slide-row1-right-X
    (list
     (matrix
      '[A C E]
      '[X B D])
     'F))
  
  (define M1-slide-column2-up-X
    (list
     (matrix
      '[A C F]
      '[B D X])
     'E))
    
  (define M1-transposed
    (matrix
     '[A B]
     '[C D]
     '[E F]))

  (check-equal? M1 M0 "R ->")

  (check-equal? (matrix-copy M1) M1 "copy")

  (check-equal? (matrix-rectangle M0) R "<-")

  (check-equal? M1 M1-make "make")
  
  (check-equal? (matrix-#rows M1) 2  "#rows")
  (check-equal? (matrix-#columns M1) 3 "#columns")

  (check-true (hash? (hash M1 0)) "hash code")

  (check-equal? (matrix-ref M1 0 1) 'C "ref")
  (check-equal? (matrix-set M1 1 1 'X) M1-set-1-1-X "set")
  (void (matrix-undo M1))
  
  (check-equal? (matrix-slide-row M1 matrix-left 1 'X) M1-slide-row1-left-X "slide 1 left X")
  (void (matrix-undo M1))
  
  (check-equal? (matrix-slide-row M1 matrix-right 1 'X) M1-slide-row1-right-X "slide 1 right X")
  (void (matrix-undo M1))
  
  (check-equal? (matrix-slide-row (first M1-slide-row1-left-X) matrix-right 1 'B) [list M1 'X] "1b")
  (void (matrix-undo (first M1-slide-row1-left-X)))
  
  (check-equal? (matrix-slide-column M1 matrix-up 2 'X) M1-slide-column2-up-X "slide 2 up X")
  (void (matrix-undo M1))

  (check-equal? (matrix-slide-column (first M1-slide-column2-up-X) matrix-down 2 'E) [list M1 'X] "2")
  (void (matrix-undo (first M1-slide-column2-up-X)))

  (check-equal? (matrix-transpose M1) M1-transposed "transpose basic")
  (check-equal? (matrix-transpose (matrix-transpose M1)) M1 "transpose o transpose")

  (check-equal? (matrix-pad M1 '[a b c d e f] #:nuwidth 4 #:nuheight 3) M1-at-3-x-4 "padding"))

(module+ test
  (define R1 (make-matrix (build-list 11 (λ (row) (build-list 11 (λ (col) [list row col]))))))

  (time (for ([i 100090]) (matrix-slide-column R1 matrix-down 1 'anyany) (matrix-undo R1) (void))))

;; ---------------------------------------------------------------------------------------------------
;; negative tests

(module+ test

  (check-exn #px"expected: pair?" matrix)
  (check-exn #px"expected: list?" (λ () (matrix (cons 1 2))))
  (check-exn #px"same-length" (λ () (matrix '[A B] '[C])))

  (check-exn #px"row index" (λ () (matrix-ref M1 3 9)))
  (check-exn #px"column index" (λ () (matrix-ref M1 0 9)))

  (check-exn #px"left-right" (λ () (matrix-slide-row M1 matrix-down 0 'x)))
  (check-exn #px"row index" (λ () (matrix-slide-row M1 matrix-left 9 'x)))

  (check-exn #px"up-down" (λ () (matrix-slide-column M1 matrix-left 0 'x)))
  (check-exn #px"column index" (λ () (matrix-slide-column M1 matrix-up 9 'x))))


