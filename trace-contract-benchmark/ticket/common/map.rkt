#lang racket

;; a data representation of the railroad map

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

(require (only-in "basic-constants.rkt" width? height? color? seg#?))
(require "connection.rkt")
(require "../../util/deps/swdev/Contracts/unique.rkt")
(require (prefix-in htdp: 2htdp/image))

;; ---------------------------------------------------------------------------------------------------
(define destination/c (and/c (list/c symbol? symbol?) (λ (x) (symbol<? (first x) (second x)))))

(define connection*/c [listof connection/c])

(define (in-cities? nodes)
  (define cities (map first nodes))
  (λ (c*)
    (for/and ([c c*])
      (define r  (and (member (connection-from c) cities) (member (connection-to c) cities)))
      (unless r
        (define m `[,(connection-from c) or ,(connection-to c) not cities: ,cities])
        (writeln m (current-error-port)))
      r)))

(define (is-city a-game-map)
  (define cities (map node-name (game-map-city-places a-game-map)))
  (λ (c) (member c cities)))

;; ---------------------------------------------------------------------------------------------------
(provide

 game-map?

 #; {type Destination  = [List City City]}
 ;; a destination card specifies two cities; there is guaranteed to be a path between them
 #; (destinaion? (list city1 city2)) #; impllies #; (symbol<? city1 city2)
 destination/c

 (contract-out
  [construct-game-map
   (->i ([w width?]
         [h height?]
	 ;; the city names are assumed to be distinct?
         [cities-and-places [listof [list/c symbol? [list/c natural? natural?]]]]
         [connections       [cities-and-places] (and/c connection*/c (in-cities? cities-and-places))])
        (#:map [map any/c])
        (r game-map?))]

  [project-game-map
   ;; project this game-map to the set of given connections
   (-> game-map? [set/c connection/c] game-map?)]


  #; {[Listof Symbol] N Width Height -> GameMap}
  ;; all symbols are pairwise distinct
  #; (construct-random-map city-names m width height)
  ;; creates a map of size `width` x `height` with the named cities and `m` connections between them
  (construct-random-map (-> width? height? (and/c (listof symbol?) unique/c) natural? game-map?))

  [game-map-width     (-> game-map? width?)]
  [game-map-height    (-> game-map? height?)]
  [game-map-png       (-> game-map? (or/c #false htdp:image?))]
  [game-map-cities    (-> game-map? [listof symbol?])]
  [game-map-locations (-> game-map? [listof [list/c symbol? (list/c natural? natural?)]])]

  [game-map-all-connections (-> game-map? (set/c connection/c))]

  ;; all cities reachable from the given one (by color and seg#)
  [game-map-connections  (-> game-map? symbol? (listof (list/c symbol? color? seg#?)))]

  ;; compute all connected pairs of cities in the given `game-map`
  [all-destinations      (-> game-map? (listof destination/c))]

  ;; are the two cities connected in the given `game-map`
  ;; (useful for projected maps when `all-d` isn't needed)
  (game-map-connected?   (->i ([g game-map?] [from (g) (is-city g)] [to (g) (is-city g)]) (r any/c)))

  ;; (used for projected maps)
  (game-map-longest-path (-> game-map? natural?))))

#;(module+ examples
  (provide vrectangle)
  (provide vtriangle-paths vtriangle-dests vtriangle-conns vtriangle-boston-seattle)
  (provide vtriangle++ kc-st vtriangle triangle-nod* triangle-source triangle))

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


(require (except-in "basic-constants.rkt" width? height? color? seg#?))
(require (prefix-in htdp: 2htdp/image))

#;(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require "../lib/get-image-from-url.rkt")
  (require rackunit))

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

;; INCLUDING THE VISUAL ELEMENTS
(struct game-map
  [;; the first four fields are essential
   width
   height
   city-places
   graph
   ;; these three fields are about memoization
   destinations paths paths-between
   ;; this one is for background visualization
   png]
  #:transparent
  #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc one two ek)
     (equal?
      (drop (reverse (vector->list (struct->vector one))) 4)
      (drop (reverse (vector->list (struct->vector two))) 4)))
   (define hash-proc  (λ _ 100))
   (define hash2-proc (λ _ 10000))])

(struct to [city color seg#] #:prefab)
(struct node [name posn] #:prefab)
(struct cord [x y] #:prefab)

;; The visual graph structure
;;
;; THE GRAPH STRUCTURE
#; {type VGraph = (game-map Natural Natural Nod* Graph)}
;;                INTERPRETATION width x height plus city specs plus connectivity
#; {type Nod*   = [Listof Node]}
#; {type Node   = (node Symbol Cord)}
#; {type Cord   = (cord N N)}
;;                INTERPRETATION what cities are called, where they are located
;;                ASSUMPTIONS, enforced by exported constructor
;;                -- the city names are distinct
;;                -- the locations are within the width x height grid
;;
#; {type Graph  = (Hashof Symbol Slice*)}
;;                INTERPRETATION maps city to all existing Slices
#; {type Slice  = (to Symbol Color Seg#)}
;;                INTERPRETATION target city, the color of the connection, and the number of segments
;;                ASSUMPTIONS, enforced by exported constructor
;;                -- cities are consistently named

;; ---------------------------------------------------------------------------------------------------
(define (plain-game-map width height city-places graph)
  (game-map width height city-places graph #false #false (hash) #false))

(define (game-map-locations g)
  (for/list ([n (game-map-city-places g)])
    `[,(node-name n) ,(rest (vector->list (struct->vector (node-posn n))))]))

;; ---------------------------------------------------------------------------------------------------
#;(module+ examples
  (define triangle-source
    `[,[connection Orlando Seattle blue 5]
      ,[connection Boston  Seattle red 3]
      ,[connection Boston  Seattle green 4]
      ,[connection Boston  Orlando white 3]
      ,[connection Boston  Orlando green 5]])

  (define kc-st '["Kansas City" "St. Louis" "green" 4])

  (define triangle++-source
    (append triangle-source `[,[connection |Kansas City| |St. Louis| green 4]]))

  (define triangle
    [hasheq 'Orlando `[,[to 'Boston 'white 3]
                       ,[to 'Boston 'green 5]
                       ,[to 'Seattle 'blue 5]]
            'Seattle `[,[to 'Orlando 'blue 5]
                       ,[to 'Boston 'red 3]
                       ,[to 'Boston 'green 4]]
            'Boston  `[,[to 'Seattle 'red 3]
                       ,[to 'Seattle 'green 4]
                       ,[to 'Orlando 'white 3]
                       ,[to 'Orlando 'green 5]]])

  (define simple-triangle
    [hasheq 'Orlando `[,[to 'Boston  'blue 3]
                       ,[to 'Seattle 'blue 3]]
            'Boston  `[,[to 'Seattle 'blue 3]
                       ,[to 'Orlando 'blue 3]]
            'Seattle `[,[to 'Boston  'blue 3]
                       ,[to 'Orlando 'blue 3]]])

  (provide simple-triangle-paths)
  (define simple-triangle-paths
    `{;; Boston <-> Seattle
      ;; ------------------
      [(Boston Seattle blue 3) (Orlando Seattle blue 3)]
      [(Boston Seattle blue 3)]
      ;; Boston <-> Orlando
      ;; ------------------
      [(Boston Orlando blue 3) (Orlando Seattle blue 3)]
      [(Boston Orlando blue 3)]
      ;; Orlando <-> Seattle
      ;; -------------------
      [(Orlando Seattle blue 3)]
      [(Boston Orlando blue 3) (Boston Seattle blue 3)]})

  (define triangle-nod*
    '[[Boston  [100 100]]
      [Orlando [30 300]]
      [Seattle [200 20]]])

  (define triangle++-nod*
    (append triangle-nod*
            '[[|St. Louis| [150 150]]
              [|Kansas City| [160 140]]]))

  (define (vtriangle-with-height h) (plain-game-map MAX-WIDTH h (list->node triangle-nod*) triangle))

  (define vtriangle (plain-game-map MAX-WIDTH MAX-WIDTH (list->node triangle-nod*) triangle))
  (define striangle (plain-game-map MAX-WIDTH MAX-WIDTH (list->node triangle-nod*) simple-triangle))

  (provide vtriangle-too-many)

  (define vtriangle-too-many
    (let* ([lettrs "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
           [names  (map (compose string->symbol string) (string->list lettrs))]
           [places (for/list ([n names] [x (in-naturals)]) `[,n [,x ,x]])]
           [cities (append places triangle-nod*)])
      (plain-game-map MAX-WIDTH MAX-WIDTH (list->node cities) triangle)))

  (define vtriangle-boston-seattle (connection Boston Seattle red 3))
  (define vtriangle-paths (all-possible-paths vtriangle))
  (define vtriangle-conns (set vtriangle-boston-seattle))
  (define vtriangle-dests
    (append (all-destinations vtriangle)
            '[[Boston SanDiego]
              [Orlando SanDiego]
              [Chicago SanDiego]
              [SanDiego Seattle]])))

;
;
;                              ;                               ;
;                              ;                      ;        ;
;                                                     ;
;   ; ;;;    ;;;;;   ;;;;    ;;;     ;;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;
;   ;;  ;    ;;     ;;  ;;     ;     ;  ;;   ;   ;    ;        ;    ;;  ;;  ;;   ;
;   ;    ;   ;      ;    ;     ;    ;    ;  ;         ;        ;    ;    ;  ;    ;
;   ;    ;   ;      ;    ;     ;    ;;;;;;  ;         ;        ;    ;    ;  ;    ;
;   ;    ;   ;      ;    ;     ;    ;       ;         ;        ;    ;    ;  ;    ;
;   ;    ;   ;      ;    ;     ;    ;       ;         ;        ;    ;    ;  ;    ;
;   ;;  ;    ;      ;;  ;;     ;     ;       ;   ;    ;        ;    ;;  ;;  ;    ;
;   ; ;;;    ;       ;;;;      ;     ;;;;;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;
;   ;                          ;
;   ;                          ;
;   ;                       ;;;
;

#;(module+ examples
  (provide project-triangle-to projected-vtriangle)

  (define project-triangle-to
    [set [connection Boston Seattle red 3] [connection Orlando Seattle blue 5]])
  (define projected-triangle
    [hasheq 'Orlando `[,[to 'Seattle 'blue 5]]
            'Seattle `[,[to 'Boston 'red 3]
                       ,[to 'Orlando 'blue 5]]
            'Boston  `[,[to 'Seattle 'red 3]]])
  (define projected-vtriangle
    (plain-game-map MAX-WIDTH MAX-WIDTH (list->node triangle-nod*) projected-triangle)))

#; {GameMap [Setof Connections] -> GameMap}

#;(module+ test
  (check-equal? (project-game-map vtriangle project-triangle-to) projected-vtriangle))

(define (project-game-map gm connections)
  (define cities (game-map-city-places gm))
  (define projected-cities
    (sort
     (set->list
      (for/fold ([s (set)]) ([c (in-set connections)])
        (define c1 (connection-from c))
        (define c1-in-cities (first (memf (λ (x) (eq? (node-name x) c1)) cities)))
        (define c2 (connection-to c))
        (define c2-in-cities (first (memf (λ (x) (eq? (node-name x) c2)) cities)))
        (set-add (set-add s c1-in-cities) c2-in-cities))) symbol<? #:key node-name))
  (define width (game-map-width gm))
  (define height (game-map-height gm))
  (plain-game-map width height projected-cities (connections->graph (set->list connections))))

;
;
;                                                                              ;
;                                     ;                               ;
;                                     ;                               ;
;     ;;;    ;;;;   ; ;;;    ;;;;   ;;;;;;   ;;;;   ;    ;    ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;
;    ;   ;  ;;  ;;  ;;   ;  ;    ;    ;      ;;  ;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;;   ;
;   ;       ;    ;  ;    ;  ;         ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ;
;   ;       ;    ;  ;    ;   ;;;;     ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ;
;   ;       ;    ;  ;    ;       ;    ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ;
;    ;   ;  ;;  ;;  ;    ;  ;    ;    ;      ;      ;   ;;   ;   ;    ;        ;    ;;  ;;  ;    ;
;     ;;;    ;;;;   ;    ;   ;;;;      ;;;   ;       ;;; ;    ;;;      ;;;   ;;;;;   ;;;;   ;    ;
;
;
;
;

(define (construct-game-map width height nod* c* #:map (a-png-map #false))
  (define gm (plain-game-map width height (list->node nod*) (connections->graph c*)))
  (when a-png-map
    (define w (htdp:image-width a-png-map))
    (define h (htdp:image-height a-png-map))
    (unless (and (= width w) (= height h))
      (error 'construct-game-map
             "the given Image has dimensions different from those specified: ~e vs ~e, ~e vs ~e"
             w width
             h height))
    (set-game-map-png! gm a-png-map))
  gm)

#; {[Listof [List Symbol [List N N]]] -> [Listof Node]}
(define (list->node nod*)
  (map (λ (x) (node (first x) (apply cord (second x)))) nod*))

#; {[Listof [List Symbol Symvol ColorSymbol Seg#]] -> Graph}
(define (connections->graph c*)
  (let* ([graph (hasheq)]
         [graph (add-one-direction graph c*)]
         [graph (add-one-direction graph (map connection-flip c*))])
    graph))

#; {[Hashof Symbol Slice] [Listof Connections] -> [Hashof Symbol Slice]}
(define (add-one-direction graph c*)
  (for*/fold ([directed-graph graph])
             ([c (group-by connection-from c*)]
              [from (in-value (connection-from (first c)))])
    (define up
      (curry append (map (λ (x) (to (connection-to x) (connection-color x) (connection-seg# x))) c)))
    (hash-update directed-graph from up '[])))

;
;
;                                ;
;                                ;
;                                ;
;    ;;;;;    ;;;   ; ;;;    ;;; ;   ;;;;   ;;;;;;            ;;;    ;;;;   ; ;;;    ;;;;
;    ;;      ;   ;  ;;   ;   ;  ;;  ;;  ;;  ;  ;  ;          ;   ;  ;;  ;;  ;;   ;  ;    ;
;    ;           ;  ;    ;  ;    ;  ;    ;  ;  ;  ;         ;       ;    ;  ;    ;  ;
;    ;       ;;;;;  ;    ;  ;    ;  ;    ;  ;  ;  ;         ;       ;    ;  ;    ;  ;;;
;    ;      ;    ;  ;    ;  ;    ;  ;    ;  ;  ;  ;         ;       ;    ;  ;    ;     ;;;
;    ;      ;    ;  ;    ;  ;    ;  ;    ;  ;  ;  ;         ;       ;    ;  ;    ;       ;    ;;
;    ;      ;   ;;  ;    ;   ;  ;;  ;;  ;;  ;  ;  ;          ;   ;  ;;  ;;  ;    ;  ;    ;    ;;
;    ;       ;;; ;  ;    ;   ;;; ;   ;;;;   ;  ;  ;           ;;;    ;;;;   ;    ;   ;;;;     ;;
;
;
;
;

(define (construct-random-map width height city-names m-connections)
  (define nodes (send (new random-nodes% [over city-names] [width width] [height height]) main))
  (define conns (send (new random-connections% [city-names city-names] [over m-connections]) main))
  (plain-game-map width height (list->node nodes) (connections->graph conns)))

(define RANDOM 10)

(define random%
  (class object% (init-field over [tries RANDOM])
    (super-new)

    (define/public (main)
      (for/fold ([so-far '()]) ([i over])
        (define next (random-1 (good? so-far)))
        (cons (make-next i next) so-far)))

    (define/public (random-1 good?)
      (let random-1 ([tries tries])
        (when (zero? tries) (err))
        (define candidate (generate))
        (if (good? candidate) candidate (random-1 (- tries 1)))))

    (define/public (generate) 'error)
    (define/public (err) 'error)
    (define/pubment ((good? so-far) x) (not (inner #false good? so-far x)))
    (define/public (make-next i next) 'error)))

(define random-nodes%
  #; (new random-nodes% [over [Listof CityName]] [width N] [height N])
  ;; generates
  #; [Listof [List CityName [List N N]]]
  ;; disttinct positions for every city name in width x height
  (class random% (init-field width height)
    (super-new)

    (define/override (generate) (list (random width) (random height)))

    (define/override (err)
      (error 'random-nodes "unable to generate more city locations in [~a x ~a]" width height))

    (define/augment (good? so-far x-y) (memf (λ (x) (equal? x-y (second x))) so-far))

    (define/override (make-next c x) (list c x))))

(define random-connections%
  #; (new random-connections% [city-names [Listof CityName]] [over N])
  ;; generates
  #; [Listof [List CityName CityName Color Seg#]]
  ;; `over` distinct connections between the specified cities
  (class random% (init-field city-names)
    (super-new)

    (define/override (generate)
      (define c1 (random-pick city-names))
      (define c2 (random-pick city-names))
      (cond
        [(equal? c1 c2) (generate)]
        [else (connection (list-cities c1 c2) (random-pick COLORS) (random-pick SEG#))]))

    (define/override (err)
      (error 'random-connections "unable to generate more connections for ~a" city-names))

    (define/augment (good? so-far a-b) (member a-b so-far))

    (define/override (make-next _ x) x)))

(define (random-pick lox)
  (list-ref lox (random (length lox))))

#;(module+ test

  (define-syntax-rule (check-random width height cities# connections# msg)
    (let* ([n->str  (compose string->symbol ~a)]
           [random1 (construct-random-map  width height (build-list cities# n->str) connections#)])
      (check-equal? (game-map-width random1) width (~a msg " width"))
      (check-equal? (game-map-height random1) height (~a msg " height"))
      (check-true (game-map-city-places-set? random1 cities#) (~a msg " location sets"))
      (check-true (game-map-connections-set? random1 connections#) (~a msg " connection sets"))))

  #; {GameMap N -> Boolean}
  ;; .. plus there are n cities
  (define (game-map-city-places-set? gm n)
    (define places (map node-posn (game-map-city-places gm)))
    (= (set-count (apply set places)) (length places) n))

  #; {GameMap N -> Boolean}
  ;; every set of connections from one city to another is a set
  ;; the total size of these sets is 2n, because the graph is undirected
  (define (game-map-connections-set? gm n)
    (define g (game-map-graph gm))

    (define sizes
      (for/list ([(from slice) (in-hash g)])
        (define L (length slice))
        (and (= (set-count (apply set slice)) L) L)))

    (and (andmap values sizes) (= (apply + sizes) (* 2 n))))

  (check-random 200 800 6 20 "random 1")
  (check-random 200 100 12 20 "random 2"))


;
;
;            ;;;     ;;;                                    ;
;              ;       ;                              ;     ;
;              ;       ;                              ;     ;
;     ;;;      ;       ;            ; ;;;     ;;;   ;;;;;;  ; ;;;    ;;;;
;    ;   ;     ;       ;            ;;  ;;   ;   ;    ;     ;;   ;  ;    ;
;        ;     ;       ;            ;    ;       ;    ;     ;    ;  ;
;    ;;;;;     ;       ;            ;    ;   ;;;;;    ;     ;    ;   ;;;;
;   ;    ;     ;       ;            ;    ;  ;    ;    ;     ;    ;       ;
;   ;   ;;     ;       ;            ;;  ;;  ;   ;;    ;     ;    ;  ;    ;
;    ;;; ;      ;;;     ;;;         ; ;;;    ;;; ;     ;;;  ;    ;   ;;;;
;                                   ;
;                                   ;
;                                   ;
;

(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket ~a)))

(define-syntax (define/memoize stx)
  (syntax-parse stx
    [(define/memoize (name:id vgraph:id x:id ...) retrieve setter)
     #:do ([define proper:id (string->symbol (~a (syntax-e #'name) "/proper"))])
     #:with proper (datum->syntax stx proper:id stx stx)
     #'(define (name vgraph x ...)
         (define ?destinations (retrieve vgraph x ...))
         (cond
           [?destinations => values]
           [else
            (define dests (proper vgraph x ...))
            (setter vgraph dests x ...)
            dests]))]))

;; ---------------------------------------------------------------------------------------------------
#; {GameMap -> N}
;; the longest path in this game map
(define (game-map-longest-path gm)
  (define paths   (all-possible-paths gm))
  (define lengths (map length paths))
  (apply max 0 lengths))

(define/memoize (all-destinations vgraph)
  game-map-destinations
  set-game-map-destinations!)

(define (all-destinations/proper vgraph)
  (define cities (game-map-cities vgraph))
  (for*/fold ([destinations '()]) ([from cities][to cities] #:when (symbol<? from to))
    (define are-there-any-paths (game-map-connected? vgraph from to))
    (if are-there-any-paths (cons (list from to) destinations) destinations)))

#; {GameMap City City -> Boolean}
(define (game-map-connected? vgraph from0 to0)
  (define graph  (game-map-graph vgraph))
  (let loop ([from from0] [seen '()])
    (cond
      [(member from seen) #false]
      [else
       (for/or ([1step (hash-ref graph from '[])])
         (match-define [to city color seg#] 1step)
         (cond
           [(symbol=? city to0) #true]
           [else (loop city (cons from seen))]))])))

;; ---------------------------------------------------------------------------------------------------
(define/memoize (all-possible-paths vgraph)
  game-map-paths
  set-game-map-paths!)

(define (all-possible-paths/proper vgraph)
  (define graph  (game-map-graph vgraph))
  (define cities (game-map-cities vgraph))
  (for*/fold ([paths '()]) ([from cities][to cities] #:when (symbol<? from to))
    (append paths (all-paths vgraph from to))))

;; ---------------------------------------------------------------------------------------------------
(define/memoize (all-paths vgraph start end)
  (λ (vgraph start end)
    (define paths-field (game-map-paths-between vgraph))
    (hash-ref paths-field (list start end) #false))
  (λ (vgraph paths start end)
    (define paths-field (game-map-paths-between vgraph))
    (set-game-map-paths-between! vgraph (hash-set paths-field (list start end) paths))))

(define (all-paths/proper vgraph start end)
  (define graph (game-map-graph vgraph))

  #; {City [Listof City] -> [Listof Path]}
  ;; find all paths from `start` to `end`, unless `start` is in `been-there0`
  ;; ACCU `been-there0` is a list of all cities seen between `start0` and `start` in `graph`
  (define (all-paths start been-there0)
    (cond
      [(member start been-there0) '()]
      [else
       (define been-there (cons start been-there0))
       (define all-steps  (hash-ref graph start '[]))
       (for/fold ([paths '()]) ([1step all-steps])
         (match-define [to city color seg#] 1step)
         (define adder (add-step start city color seg#))
         (cond
           [(symbol=? city the-end) (append (list (adder '[])) paths)]
           [else (append (map adder (all-paths city been-there)) paths)]))]))

  #; {City City Color Seg# -> [Path ->  Path]}
  (define (add-step from to color seg#)
    (define 1step (append (list-cities from to) (list color seg#)))
    (λ (path) (cons 1step path)))

  (match-define (list the-start the-end) (list-cities start end))
  (all-paths the-start '[]))

;; ---------------------------------------------------------------------------------------------------
(define (game-map-connections gm city)
  (define connections (hash-ref (game-map-graph gm) city '[]))
  (map (λ (x) (rest (vector->list (struct->vector x)))) connections))

;; ---------------------------------------------------------------------------------------------------
(define (game-map-all-connections graph)
  (for/fold ([s (set)]) ([from (game-map-cities graph)])
    (set-union
     s
     (for/set ([c (game-map-connections graph from)])
       (match-define [list to color seg#] c)
       (connection (list-cities from to) color seg#)))))

;; ---------------------------------------------------------------------------------------------------
(define (game-map-cities graph) (map node-name (game-map-city-places graph)))

;
;
;
;     ;                       ;
;     ;                       ;
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;
;     ;     ;    ;  ;    ;    ;     ;    ;
;     ;     ;;;;;;  ;         ;     ;
;     ;     ;        ;;;;     ;      ;;;;
;     ;     ;            ;    ;          ;
;     ;     ;;   ;  ;    ;    ;     ;    ;
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;
;
;
;
;

#;(module+ examples ;; more

  (provide striangle vtriangle-with-height vdisconnected)

  (define vrectangle
    (plain-game-map
     800
     800
     '(#s(node |San Diego| #s(cord 176 571))
       #s(node Orlando     #s(cord 715 528))
       #s(node Boston      #s(cord 793 201))
       #s(node NYC         #s(cord 693 301))
       #s(node Seattle     #s(cord 131 168)))
     '#hash((Boston      . (#s(to Orlando white 5)
                            #s(to NYC white 3)
                            #s(to Seattle white 3)))
            (NYC         . (#s(to Boston white 3)))
            (Orlando     . (#s(to Boston white 5)
                            #s(to |San Diego| red 5)
                            #s(to |San Diego| blue 5)))
            (|San Diego| . (#s(to Orlando red 5)
                            #s(to Orlando blue 5)
                            #s(to Seattle white 4)))
            (Seattle     . (#s(to Boston white 3)
                            #s(to |San Diego| white 4))))))

  (define vdisconnected
    (plain-game-map
     800
     800
     '(#s(node |San Diego| #s(cord 176 571))
       #s(node Orlando     #s(cord 715 528))
       #s(node Boston      #s(cord 793 201))
       #s(node NYC         #s(cord 693 301))
       #s(node Seattle     #s(cord 131 168)))
     '#hash())))

#;(module+ test


  (check-equal? (game-map-longest-path (plain-game-map 100 100 '[] (hash))) 0)
  (check-equal? (game-map-longest-path vtriangle) 2)

  ;; -------------------------------------------------------------------------------------------------
  ;; construction

  (check-equal? (construct-game-map MAX-WIDTH MAX-WIDTH triangle-nod* triangle-source) vtriangle)


  ;; ------------------------------------------------------------------------------------------------
  ;; tests for connections->graph

  (define (->set g)
    (map (λ (x) (list (first x) (apply set (rest x)))) (hash->list g)))

  (check-equal? (->set (connections->graph triangle-source)) (->set triangle))

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for all-aths

  (define-syntax-rule (dev-null e) (parameterize ([current-error-port (open-output-string)]) e))

  (check-equal? (apply set (all-paths vtriangle 'Seattle 'Boston))
                [set `[[Boston Seattle green 4]]
                     `[[Boston Seattle red 3]]
                     `[[Boston Orlando green 5]
                       [Orlando Seattle blue 5]]
                     `[[Boston Orlando white 3]
                       [Orlando Seattle blue 5]]])

  (check-equal? (apply set (all-possible-paths striangle))
                (apply set simple-triangle-paths))

  (check-equal?
   (apply set (all-possible-paths vtriangle))
   (set-union
    (apply set (all-paths vtriangle 'Boston 'Seattle))
    (apply set (all-paths vtriangle 'Boston 'Orlando))
    (apply set (all-paths vtriangle 'Orlando 'Seattle))))

  ;; -------------------------------------------------------------------------------------------------
  ;; for game-map-connections, all of them
  (check-true (subset? (game-map-all-connections vtriangle) (list->set triangle-source))
             "game-map-connection all")

  (check-equal? (all-destinations vdisconnected) '[])

  (check-equal? (apply set (all-destinations vrectangle))
                (apply set '[[Boston |San Diego|]
                             [Boston Seattle]
                             [Boston Orlando]
                             [Boston NYC]
                             ; [NYC Boston]
                             [NYC |San Diego|]
                             [NYC Orlando]
                             [NYC Seattle]
                             [Orlando |San Diego|]
                             [Orlando Seattle]
                             [|San Diego| Seattle]])))

#;(module+ test
  (define file-path "../../../Courses/21SwDev/Source/Images/map.png")
  (define the-map    (png-from-url file-path))
  (define scaled-map (htdp:scale .8 the-map))
  (define scaled-height (htdp:image-height scaled-map))
  (check-equal?
   (construct-game-map MAX-WIDTH scaled-height triangle-nod* triangle-source #:map scaled-map)
   (vtriangle-with-height scaled-height))

  (check-exn exn:fail?
             (λ ()
               (construct-game-map MAX-WIDTH MAX-WIDTH triangle-nod* triangle-source #:map the-map))))

#;(module+ examples
  (define vtriangle++ (construct-game-map MAX-WIDTH MAX-WIDTH triangle++-nod* triangle++-source)))
