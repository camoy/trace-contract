#lang racket/base

(require racket/contract
         pict
         racket/bool
         "trace.rkt"
         "private/visualizer-drawing.rkt")

(provide
 (contract-out
  [timeline-pict (->i ([indexed-fevents (listof indexed-future-event?)])
                      (#:x [x (or/c #f exact-nonnegative-integer?)]
                       #:y [y (or/c #f exact-nonnegative-integer?)]
                       #:width [width (or/c #f exact-nonnegative-integer?)]
                       #:height [height (or/c #f exact-nonnegative-integer?)]
                       #:selected-event-index [i (or/c #f exact-nonnegative-integer?)])
                      #:pre
                      (x y width height)
                      (implies (or x y width height)
                               (and x y width height))
                      [p pict?])]))
