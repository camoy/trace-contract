#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide dc-installed?
         dc-install/c
         dc-next/c

         dc/c
         dc<%>
         dc-checked<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         automata/machine
         automata/re
         automata/re-ext
         contract-etc
         racket/class
         racket/contract
         racket/draw/private/bitmap
         racket/draw/private/brush
         racket/draw/private/color
         racket/draw/private/dc-path
         racket/draw/private/font
         racket/draw/private/gl-context
         racket/draw/private/pen
         racket/draw/private/point
         racket/function
         racket/set
         trace-contract/benchmarks/util/measure
         "brush.rkt"
         "cache.rkt"
         "pen.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; dummy value to avoid cycles via "region.rkt"
(define region% object%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attribute

(define dc-install-key (make-attribute))
(define dc-status-key (make-attribute))

(define STATUS-RE
  (re (seq/close 'start-doc
                 (star (seq/close 'start-page (star 'draw) 'end-page))
                 'end-doc)))

(define-modifiable
  #:level base
  (define (dc-init/c bmp re) any/c)
  (define (dc-install/c _) any/c)
  (define (dc-next/c _) any/c)
  (define dc-installed? any/c)
  (define dc-draw-ready? any/c)

  #:level noop
  (define (dc-init/c bmp re)
    (attribute/c object?
                 dc-install-key bmp
                 dc-status-key re))
  (define (dc-install/c bmp)
    (attribute-update/c dc-install-key (λ (_) bmp)))
  (define (dc-next/c sym)
    (and/c (attribute-update/c dc-status-key id)
           (attribute-satisfies/c dc-status-key always-true)))
  (define dc-installed? (attribute-satisfies/c dc-install-key always-true))
  (define dc-draw-ready? (and/c dc-installed? (dc-next/c 'draw)))

  #:level check
  (define (dc-init/c bmp re)
    (attribute/c object?
                 dc-install-key bmp
                 dc-status-key re))
  (define (dc-install/c bmp)
    (attribute-update/c dc-install-key (λ (_) bmp)))
  (define (dc-next/c sym)
    (and/c (attribute-update/c dc-status-key (λ (a) (a sym)))
           (attribute-satisfies/c dc-status-key (λ (m) (machine-accepting? m)))))
  (define dc-installed? (attribute-satisfies/c dc-install-key id))
  (define dc-draw-ready? (and/c dc-installed? (dc-next/c 'draw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dc<%>

(define (dc/c #:bitmap [bmp #t] #:re [re STATUS-RE])
  (and/c
   (is-a?/c dc<%>)
   (dc-init/c bmp re)))

(define dc<%>
  (interface ()
    cache-font-metrics-key
    clear
    copy
    draw-arc
    draw-bitmap
    draw-bitmap-section
    draw-ellipse
    draw-line
    draw-lines
    draw-path
    draw-point
    draw-polygon
    draw-rectangle
    draw-rounded-rectangle
    draw-spline
    draw-text
    end-doc
    end-page
    erase
    flush
    get-alpha
    get-background
    get-backing-scale
    get-brush
    get-char-height
    get-char-width
    get-clipping-region
    get-device-scale
    get-font
    get-gl-context
    get-initial-matrix
    get-origin
    get-pen
    get-rotation
    get-scale
    get-size
    get-smoothing
    get-text-background
    get-text-extent
    get-text-foreground
    get-text-mode
    get-transformation
    glyph-exists?
    ok?
    resume-flush
    rotate
    scale
    set-alignment-scale
    set-alpha
    set-background
    set-brush
    set-clipping-rect
    set-clipping-region
    set-font
    set-initial-matrix
    set-origin
    set-pen
    set-rotation
    set-scale
    set-smoothing
    set-text-background
    set-text-foreground
    set-text-mode
    set-transformation
    start-doc
    start-page
    suspend-flush
    transform
    translate
    try-color))

(define dc-checked<%>
  (interface (dc<%>)
    [cache-font-metrics-key (-> dc-installed? exact-integer?)]
    [clear (-> dc-installed? void?)]
    [copy (-> dc-installed? real? real?
              (and/c real? (not/c negative?))
              (and/c real? (not/c negative?))
              real? real?
              void?)]
    [draw-arc (-> dc-draw-ready? real? real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  real? real?
                  void?)]
    [draw-bitmap (->* (dc-draw-ready?
                       (is-a?/c bitmap%)
                       real? real?)
                      ((or/c 'solid 'opaque 'xor)
                       (is-a?/c color%)
                       (or/c (is-a?/c bitmap%) #f))
                      boolean?)]
    [draw-bitmap-section (->* (dc-draw-ready?
                               (is-a?/c bitmap%)
                               real? real?
                               real? real?
                               (and/c real? (not/c negative?))
                               (and/c real? (not/c negative?)))
                              ((or/c 'solid 'opaque 'xor)
                               (is-a?/c color%)
                               (or/c (is-a?/c bitmap%) #f))
                              boolean?)]
    [draw-ellipse (-> dc-draw-ready? real? real?
                      (and/c real? (not/c negative?))
                      (and/c real? (not/c negative?))
                      void?)]
    [draw-line (-> dc-draw-ready? real? real?
                   real? real?
                   void?)]
    [draw-lines (->* (dc-draw-ready?
                      (or/c (listof (is-a?/c point%))
                            (listof (cons/c real? real?))))
                     (real? real?)
                     void?)]
    [draw-path (->* (dc-draw-ready?
                     (is-a?/c dc-path%))
                    (real? real? (or/c 'odd-even 'winding))
                    void?)]
    [draw-point (-> dc-draw-ready? real? real? void?)]
    [draw-polygon (->* (dc-draw-ready?
                        (or/c (listof (is-a?/c point%))
                              (listof (cons/c real? real?))))
                       (real? real? (or/c 'odd-even 'winding))
                       void?)]
    [draw-rectangle (-> dc-draw-ready? real? real?
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        void?)]
    [draw-rounded-rectangle (->* (dc-draw-ready?
                                  real? real?
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?)))
                                  (real?)
                                  void?)]
    [draw-spline (-> dc-draw-ready? real? real? real?
                     real? real? real?
                     void?)]
    [draw-text (->* (dc-draw-ready? string? real? real?)
                    (any/c exact-nonnegative-integer? real?)
                    void?)]
    [end-doc (-> (and/c dc-installed? (dc-next/c 'end-doc)) void?)]
    [end-page (-> (and/c dc-installed? (dc-next/c 'end-page)) void?)]
    [erase (-> dc-installed? void?)]
    [flush (-> dc-installed? void?)]
    [get-alpha (-> dc-installed? real?)]
    [get-background (-> dc-installed? (is-a?/c color%))]
    [get-backing-scale (-> dc-installed? (>/c 0.0))]
    [get-brush (let ([c/c (cache/c*)])
                 (->i ([dc dc-installed?])
                      [res (dc) (c/c (λ () (brush/c #:installed (set dc))))]))]
    [get-char-height (->m (and/c real? (not/c negative?)))]
    [get-char-width (->m (and/c real? (not/c negative?)))]
    [get-clipping-region (-> dc-installed? (or/c (is-a?/c region%) #f))]
    [get-device-scale (-> dc-installed? (values (and/c real? (not/c negative?))
                                   (and/c real? (not/c negative?))))]
    [get-font (-> dc-installed? (is-a?/c font%))]
    [get-gl-context (-> dc-installed? (or/c (is-a?/c gl-context<%>) #f))]
    [get-initial-matrix (-> dc-installed? (vector/c real? real? real?
                                       real? real? real?))]
    [get-origin (-> dc-installed? (values real? real?))]
    [get-pen (let ([c/c (cache/c*)])
               (->i ([dc dc-installed?])
                    [res (dc) (c/c (λ () (pen/c #:installed (set dc))))]))]
    [get-rotation (-> dc-installed? real?)]
    [get-scale (-> dc-installed? (values real? real?))]
    [get-size (-> dc-installed? (values (and/c real? (not/c negative?))
                           (and/c real? (not/c negative?))))]
    [get-smoothing (-> dc-installed? (or/c 'unsmoothed 'smoothed 'aligned))]
    [get-text-background (-> dc-installed? (is-a?/c color%))]
    [get-text-extent (->*m (string?)
                           ((or/c (is-a?/c font%) #f)
                            any/c
                            exact-nonnegative-integer?)
                           (values
                            (and/c real? (not/c negative?))
                            (and/c real? (not/c negative?))
                            (and/c real? (not/c negative?))
                            (and/c real? (not/c negative?))))]
    [get-text-foreground (-> dc-installed? (is-a?/c color%))]
    [get-text-mode (-> dc-installed? (or/c 'solid 'transparent))]
    [get-transformation (-> dc-installed? (vector/c (vector/c real? real? real?
                                                 real? real? real?)
                                       real? real? real? real? real?))]
    [glyph-exists? (-> dc-installed? char? boolean?)]
    [ok? (-> dc-installed? boolean?)]
    [resume-flush (-> dc-installed? void?)]
    [rotate (-> dc-installed? real? void?)]
    [scale (-> dc-installed? real? real? void?)]
    [set-alignment-scale (-> dc-installed? (>/c 0.0) void?)]
    [set-alpha (-> dc-installed? real? void?)]
    [set-background (-> dc-installed? (or/c (is-a?/c color%) string?) void?)]
    [set-brush
     ;; TODO: This is awkward.
     (let ([dc-contract
            (and/c
             dc-installed?
             (self/c
              (λ (self)
                (elementof/c
                 (brush-uninstall/c self)
                 (λ (self) (send self get-brush))))))])
       (case->i (->i ([dc dc-contract]
                      [br (dc) (brush-install/c dc)])
                     [res void?])
                (-> dc-contract
                    (or/c (is-a?/c color%) string?)
                    brush-style/c
                    void?)))]
    [set-clipping-rect (-> dc-installed? real? real?
                           (and/c real? (not/c negative?))
                           (and/c real? (not/c negative?))
                           void?)]
    [set-clipping-region (-> dc-installed? (or/c (is-a?/c region%) #f) void?)]
    [set-font (-> dc-installed? (is-a?/c font%) void?)]
    [set-initial-matrix (-> dc-installed? (vector/c real? real? real?
                                       real? real? real?)
                             void?)]
    [set-origin (-> dc-installed? real? real? void?)]
    ;; TODO: just like `set-brush`
    [set-pen
     (let ([dc-contract
            (and/c
             (self/c
              (λ (self)
                (elementof/c
                 (pen-uninstall/c self)
                 (λ (self) (send self get-pen)))))
             dc-installed?)])
       (case->i
        (->i ([self dc-contract]
              [pen (self) (pen-install/c self)])
             [res void?])
        (-> dc-contract
            (or/c (is-a?/c color%) string?)
            real?
            pen-style/c
            void?)))]
    [set-rotation (-> dc-installed? real? void?)]
    [set-scale (-> dc-installed? real? real? void?)]
    [set-smoothing (-> dc-installed? (or/c 'unsmoothed 'smoothed 'aligned) void?)]
    [set-text-background (-> dc-installed? (or/c (is-a?/c color%) string?) void?)]
    [set-text-foreground (-> dc-installed? (or/c (is-a?/c color%) string?) void?)]
    [set-text-mode (-> dc-installed? (or/c 'solid 'transparent) void?)]
    [set-transformation (-> dc-installed? (vector/c (vector/c real? real? real?
                                                 real? real? real?)
                                       real? real? real? real? real?)
                             void?)]
    [start-doc (-> (and/c dc-installed? (dc-next/c 'start-doc)) string? void?)]
    [start-page (-> (and/c dc-installed? (dc-next/c 'start-page)) void?)]
    [suspend-flush (-> dc-installed? void?)]
    [transform (-> dc-installed? (vector/c real? real? real? real? real? real?)
                    void?)]
    [translate (-> dc-installed? real? real? void?)]
    [try-color (-> dc-installed? (is-a?/c color%) (is-a?/c color%) void?)]))
