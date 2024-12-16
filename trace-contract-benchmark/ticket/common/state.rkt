#lang racket

;; representation of a player's knowledge about the game

(require "map.rkt")

;                                                   
;                                                   
;                                        ;          
;                                        ;          
;    ;;;   ;   ;  ;;;;    ;;;    ;;;;  ;;;;;   ;;;  
;   ;;  ;   ; ;   ;; ;;  ;; ;;   ;;  ;   ;    ;   ; 
;   ;   ;;  ;;;   ;   ;  ;   ;   ;       ;    ;     
;   ;;;;;;   ;    ;   ;  ;   ;   ;       ;     ;;;  
;   ;       ;;;   ;   ;  ;   ;   ;       ;        ; 
;   ;       ; ;   ;; ;;  ;; ;;   ;       ;    ;   ; 
;    ;;;;  ;   ;  ;;;;    ;;;    ;       ;;;   ;;;  
;                 ;                                 
;                 ;                                 
;                 ;                                 

(provide

 #; {PlayerState Connection -> Player}
 ;; ASSUME the action is legal
 ii-acquire

 #; {PlayerState [Listof Color] -> PlayerState}
 ii+cards

 #; {Player -> Boolean}
 ii-final?
 
 #; {MePlayer -> N}
 ii-conn-score

 #; {PlayerState GameMap -> Integer}
 ;; the map is the projection of the game map to the player
 ii-destinations-connected 

 #; {type Connection  = [list City City Color Length]}
 ;; a connection between two cities has a color and a length

 #; {Map PlayerState -> [Setof Connection]}
 all-available-connections

 #; {PlayerState GameMap Connection -> Boolean}
 legal-action?

 #; {MePlayer[False] X -> MePlayer[X]}
 ii+payload
 
 ii? 
 (contract-out
  [ii (-> destination/c destination/c natural? hash? [set/c connection/c] any/c ii?)])

 ii-destination1
 ii-destination2
 ii-rails
 ii-cards
 ii-connections
 ii-payload
 ii-payload--
 
 pstate?
 (contract-out
  (pstate (-> ii? [listof [set/c connection/c]] pstate?)))
 pstate-I
 pstate-others)

#;(module+ examples
  (provide pstate1 pstate2 pstate-play pstate-final pstate-play+
           #; {Color N -> PlayerState : like pstate2, different color count for c}
           like-pstate2 
           conns0 conns1))

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

(require "basic-constants.rkt")
(require "connection.rkt")
(require "map.rkt")

#;(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require (submod "map.rkt" examples))
  (require rackunit))

;                                                                          
;                                                                          
;                                    ;;;                                   
;                                      ;                                   
;                                      ;                                   
;   ;;;;;;   ;;;;           ; ;;;      ;      ;;;   ;    ;   ;;;;    ;;;;  
;   ;  ;  ; ;    ;          ;;  ;;     ;     ;   ;   ;  ;;  ;    ;   ;;  ; 
;   ;  ;  ; ;;;;;;          ;    ;     ;         ;   ;  ;   ;;;;;;   ;     
;   ;  ;  ; ;               ;    ;     ;     ;;;;;   ;  ;   ;        ;     
;   ;  ;  ; ;               ;    ;     ;    ;    ;    ; ;   ;        ;     
;   ;  ;  ; ;;   ;          ;;  ;;     ;    ;   ;;    ;;    ;;   ;   ;     
;   ;  ;  ;  ;;;;;          ; ;;;       ;;;  ;;; ;     ;     ;;;;;   ;     
;                           ;                          ;                   
;                           ;                         ;                    
;                           ;                        ;;                    
;                                                                          

(struct ii [destination1 destination2 rails cards connections payload] #:transparent)
#; {type [MePlayer X] = (ii Desitination Destination Natural [Hash Color Natural] Player X)}
#; {type Player       = [Setof Connection]}
;; the two destination cards, the rails left, the colored cards, and this player's possessions

(define (ii-payload-- ii-player)
  (struct-copy ii ii-player [payload #false]))

(define (ii-acquire ii-player c)
  (define seg#  (connection-seg# c))
  (define color (connection-color c))
  (match-define (ii d-1 d-2 rails-left cards connections xplayer) ii-player)
  (ii d-1 d-2 (- rails-left seg#) (-cards cards color seg#) (set-add connections c) xplayer))

#; {[Hash Color N] Color N -> [Hash Color N]}
(define (-cards cards0 color n)
  (if (= (hash-ref cards0 color 0) n)
      (hash-remove cards0 color)
      (hash-update cards0 color (λ (old) (- old n)))))

(define (ii+cards ii-player new-cards)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (define cards1 (for/fold ([cards cards0]) ([c new-cards]) (hash-update cards c add1 0)))
  (ii d-1 d-2 rails-left cards1 connections xplayer))

(define (ii-final? ii-player)
  (< (ii-rails ii-player) RAILS-MIN))

(define (ii-conn-score ii-player)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (for/sum ([c connections]) (connection-seg# c)))

(define (ii-destinations-connected ii-player gm)
  (define cities (game-map-cities gm))
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (define (plus-minus-points dest)
    (define from (first dest))
    (define to   (second dest))
    (define any-path-connection
      (and (member from cities)
           (member to   cities)
           (game-map-connected? gm to from)))
    (if any-path-connection POINTS-PER (- POINTS-PER)))
  (+ (plus-minus-points d-1) (plus-minus-points d-2)))

(define (ii+payload ii-player pl)
  (when (ii-payload ii-player) (error 'ii+payload "payload already exists ~e" (ii-payload ii-player)))
  (struct-copy ii ii-player [payload pl]))

#; {IPlayer Connection -> Boolean}
(define (ii-can-acquire-and-occupy? ii-player c)
  (and (ii-can-acquire? ii-player c) (ii-can-occupy? ii-player c)))

#; {IPlayer Connection -> Boolean}
(define (ii-can-occupy? ii-player c)
  (>= (ii-rails ii-player) (connection-seg# c)))

#; {IPlayer Connection -> Boolean}
(define (ii-can-acquire? ii-player c)
  (define colored-cards-available (hash-ref (ii-cards ii-player) (connection-color c) 0))
  (define colored-cards-needed (connection-seg# c))
  (>= colored-cards-available colored-cards-needed))

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

#;(module+ examples
  (provide ii- cards1 cards2 ii-final ii-play path01)
  
  (define cards1 (hasheq 'green 5))
  (define cards2 (hasheq 'green 5 'blue 7 'red 6))

  (define orl-sea [connection Orlando Seattle blue 5])
  (define bos-sea [connection Boston Seattle red 3])
  (define conns0 (set orl-sea))
  (define conns1 (set bos-sea))
  (define path01 (list bos-sea orl-sea))
  
  (define (ii- cards1) (ii '(Boston Seattle) '(Boston Orlando) 40 cards1 conns0 #f))
  (define (ii-r r (cards2 cards2) (conns0 conns0))
    (ii '(Boston Seattle) '(Boston Orlando) r cards2 conns0 #f))
  
  (define ii-final (ii-r (- RAILS-MIN 1) (-cards cards2 'red 3) (set-union conns0 conns1)))
  (define ii-play  (ii-r (+ RAILS-MIN 2)))
  (define ii-play+ (let* ([i (ii-r (+ RAILS-MIN 2))]
                          [i (struct-copy ii i [destination1 '[Boston |Kansas City|]])])
                     i)))

#;(module+ test

  (define gm-ii-final (project-game-map vtriangle (set-union conns0 conns1)))
  (define gm-ii-play  (project-game-map vtriangle conns0))
  (check-equal? (ii-destinations-connected ii-final gm-ii-final) (* 2 POINTS-PER))
  (check-equal? (ii-destinations-connected ii-play  gm-ii-play)  (* -2 POINTS-PER))

  (check-true (ii-final? ii-final))
  (check-false (ii-final? ii-play))

  (define blues (make-list 7 'blue))
  (check-equal? (ii+cards (ii+cards (ii- cards1) blues) (make-list 6 'red)) (ii- cards2))

  (check-equal? (ii-acquire ii-play [connection Boston Seattle red 3]) ii-final)
  (check-equal? (ii-conn-score ii-play) 5))

;                                                  
;                                                  
;                                                  
;                     ;               ;            
;                     ;               ;            
;   ; ;;;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
;   ;;  ;;  ;    ;    ;      ;   ;    ;     ;    ; 
;   ;    ;  ;         ;          ;    ;     ;;;;;; 
;   ;    ;   ;;;;     ;      ;;;;;    ;     ;      
;   ;    ;       ;    ;     ;    ;    ;     ;      
;   ;;  ;;  ;    ;    ;     ;   ;;    ;     ;;   ; 
;   ; ;;;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;; 
;   ;                                              
;   ;                                              
;   ;                                              
;                                                  

(struct pstate [I others] #:transparent)
#; {type PlayerState  = (pstate [MePlayer Any] [Listof Player])}
;; what the player knows about itself and others 

#;(module+ examples

  (provide pstate0)

  (define pstate0 (pstate (ii- cards1) '[]))

  (define pstate1 (pstate (ii- cards1) (list conns1)))
  
  (define (like-pstate2 c n) (pstate (ii- (hash-set cards2 c n)) (list conns1)))

  (define pstate2 (like-pstate2 'green 5))

  (define pstate-play+ (pstate ii-play+ (list conns0)))
  (define pstate-play  (pstate ii-play (list conns0)))
  (define pstate-final (pstate ii-final (list conns0 conns1))))

#; {Map PlayerState -> [Setof Connections]}
;; determine the connections the active (`my`) player can still acquire
;; given the fixed game map and what `my` and `others` already own 
(define (all-available-connections gm ps)
  (define total  (game-map-all-connections gm))
  (define mine   (my-connections ps))
  (define others (other-s-connections ps))
  (define bought (set-union mine others))
  (set-subtract total bought))

#; {PlayerState -> [Setof Connection]}
(define (other-s-connections ps)
  (define bought (pstate-others ps))
  (if (empty? bought) (set) (apply set-union bought)))

#; {PlayerState -> [Setof Connection]}
(define (my-connections ps)
  (ii-connections (pstate-I ps)))

(define TERMINATION# 3)

#; {PlayerState N -> N}
(define (termination ps rails0)
  (match-define [pstate I others] ps)
  (define my-acquisitions (ii-connections I))
  (define rails-consumed  (map rails-spent (cons my-acquisitions others)))
  (- rails0 (apply max rails-consumed)))

(define (rails-spent connections)
  (for/sum ([c connections]) (connection-seg# c)))

#; {PlayerState GameMap Connection -> Boolean}
;; can this player acquire the specified connection 
(define (legal-action? ps gm c)
  (define available  (all-available-connections gm ps))
  (if (set-member? available c)
      (ii-can-acquire-and-occupy? (pstate-I ps) c)
      #false))

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

  (check-equal? (termination pstate1 45) 40)
  (check-equal? (termination pstate1 6) 1)
  (check-equal? (termination pstate1 8) 3)
  
 
  (check-equal? 
   (all-available-connections vtriangle pstate1)
   (set-subtract (game-map-all-connections vtriangle) conns0 conns1))

  (check-equal? 
   (all-available-connections vtriangle pstate0)
   (set-subtract (game-map-all-connections vtriangle) conns0) "there are no others!")
  
  (check-false (legal-action? pstate1 vtriangle (connection Boston Seattle red  3)))
  (check-true (legal-action? pstate1 vtriangle (connection Boston Orlando green  5))))
