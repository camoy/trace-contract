#lang racket

;; basic constants and functions for the game

(provide
 width?
 height?
 city?

 #; {Symbol Symbol -> [List Symbol Symbol] :: ordered via symbol<?}
 list-cities

 color?
 seg#?

 COLORS
 SEG#
 MIN-WIDTH  MAX-WIDTH
 MIN-HEIGHT MAX-HEIGHT

 ;; for homework only
 CITY-LENGTH
 CITY-NAME

 DESTS-PER
 PICKS-PER
 RAILS-PER
 RAILS-MIN
 CARD0-PER
 CARDS-PER

 POINTS-PER
 LONG-PATH

 CITY#
 CONN#

 MIN-PLAYER-PER-GAME
 MAX-PLAYER-PER-GAME

 CARDS-PER-GAME
 MAX-PLAYER-NAME
 PLAYER-NAME-PX

 MORE
 more?)

;; -----------------------------------------------------------------------------
(define CITY# 20)
(define CONN# 40)

(define COLORS '[red blue green white])

(define (color? x) (member (if (string? x) (string->symbol (string-downcase x)) x) COLORS))

(define SEG# '[3 4 5])

(define (seg#? x) (member x SEG#))

(define CITY-NAME "[a-zA-Z0-9\\ \\.\\,]+")
(define CITY-LENGTH 25)
(define pxCITY (pregexp CITY-NAME))

(define (city? s)
  (and (or (string? s) (symbol? s))
       (let ([s (if (string? s) s (~a s))])
         (<= 1 (string-length s) CITY-LENGTH)
         (let ([m (regexp-match pxCITY s)])
           (and (cons? m) (equal? (first m) s))))))

(define (list-cities start end)
  (if (symbol<? start end) (list start end) (list end start)))

(define MIN-WIDTH 10)
(define MAX-WIDTH 800)
(define MIN-HEIGHT 10)
(define MAX-HEIGHT 800)

(define (width? x)
  (and (integer? x) (positive? x) (<= MIN-WIDTH x MAX-WIDTH)))

(define (height? x)
  (and (integer? x)  (positive? x) (<= MIN-HEIGHT x MAX-HEIGHT)))

(define RAILS-PER 45) ;; the number of rails a player receives during set-up
(define CARD0-PER  4) ;; the colored cards a player receives during set-up
(define CARDS-PER  2) ;; the number of colored cards a player receives during a turn
(define DESTS-PER  2) ;; the destinations a player must pick during set-up from ..
(define PICKS-PER  5) ;; .. the number of cards it receives during set-up
(define RAILS-MIN  3) ;; the minimum number of rails a player must own to continue playing

(define POINTS-PER 10)
(define LONG-PATH  10)

(define MIN-PLAYER-PER-GAME 2)
(define MAX-PLAYER-PER-GAME 8)

(define CARDS-PER-GAME 250)
(define MAX-PLAYER-NAME 50)
(define PLAYER-NAME-PX #px"[a-zA-Z]")

(define MORE "more cards")

(define (more? x) (eq? x MORE))

;; -----------------------------------------------------------------------------
#;(module+ test
  (require rackunit)

  (define DC  "Washington, D.C.")
  (check-equal? (regexp-match pxCITY DC) `[,DC])
  (check-true (city? DC)))
