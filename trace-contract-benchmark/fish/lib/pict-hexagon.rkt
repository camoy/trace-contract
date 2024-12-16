#lang racket

;; hexagons in pict format, derived from image format

(provide
 hexagon
 filled-hexagon)

;; ---------------------------------------------------------------------------------------------------
(require (prefix-in image: "hexagon.rkt"))
(require pict/convert)
(require pict)

;; ---------------------------------------------------------------------------------------------------
(define (hexagon size #:border-color (color "white") #:brder-width (width #false))
  (define tile (pict-convert (image:hexagon size 'outline color)))
  tile)

(define (filled-hexagon size
                        #:draw-border? (border #true)
                        #:color        (color "white")
                        #:border-color (border-color "white")
                        #:brder-width  (width #false))
  (define inner-tile (pict-convert (image:hexagon size'solid color)))
  (define outer-tile (pict-convert (image:hexagon size'outline border-color)))
  (cc-superimpose inner-tile outer-tile))
