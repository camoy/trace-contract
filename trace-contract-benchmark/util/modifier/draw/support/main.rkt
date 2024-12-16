#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         transformation-vector/c
         make-color/c
         make-brush/c
         make-pen/c
         color%/c
         point%/c
         font%/c
         pen%/c
         pen-list%/c
         brush%/c
         brush-list%/c
         linear-gradient%/c
         radial-gradient%/c
         bitmap-dc%/c
         post-script-dc%/c
         pdf-dc%/c
         svg-dc%/c
         record-dc%/c
         region%/c
         dc-path%/c
         gl-config%/c
         bitmap-kind/c
         bitmap/c
         bitmap%/c
         color-database/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         automata/machine
         automata/re
         automata/re-ext
         contract-etc
         racket/contract/base
         racket/class
         racket/draw/private/color
         racket/draw/private/point
         racket/draw/private/font
         racket/draw/private/font-dir
         racket/draw/private/font-syms
         racket/draw/private/pen
         racket/draw/private/brush
         racket/draw/private/gradient
         racket/draw/private/region
         racket/draw/private/bitmap
         racket/draw/private/dc-path
         racket/draw/private/bitmap-dc
         racket/draw/private/post-script-dc
         racket/draw/private/ps-setup
         racket/draw/private/svg-dc
         racket/draw/private/gl-config
         racket/draw/private/gl-context
         racket/function
         racket/match
         racket/set
         "bitmap.rkt"
         "brush.rkt"
         "cache.rkt"
         "dc-intf.rkt"
         "pen.rkt"
         trace-contract/benchmarks/util/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; dummy values to avoid cycles
(define-values (frame% dialog%) (values object% object%))

(define make-brush/c
  (->* ()
       (#:color (or/c string? (is-a?/c color%))
        #:style brush-style/c
        #:stipple (or/c #f (is-a?/c bitmap%))
        #:gradient (or/c #f
                         (is-a?/c linear-gradient%)
                         (is-a?/c radial-gradient%))
        #:transformation (or/c #f transformation-vector/c)
        #:immutable? any/c)
       (brush/c)))

(define make-pen/c
  (->* ()
       (#:color (or/c string? (is-a?/c color%))
        #:width (real-in 0 255)
        #:style pen-style/c
        #:cap pen-cap-style/c
        #:join pen-join-style/c
        #:stipple (or/c #f (is-a?/c bitmap%))
        #:immutable? any/c)
       (pen/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color%

(define color-immutable-key (make-attribute))

(define-modifiable
  #:level base
  (define (color-init/c imm?) any/c)
  (define color-mutable? any/c)

  #:level noop
  (define (color-init/c imm?)
    (attribute/c object? color-immutable-key imm?))
  (define color-mutable?
    (attribute-satisfies/c color-immutable-key always-true))

  #:level check
  (define (color-init/c imm?)
    (attribute/c object? color-immutable-key imm?))
  (define color-mutable?
    (attribute-satisfies/c color-immutable-key (λ (x) (not x)))))

;; We need to use `cache/c` to prevent an unreasonable amount of wrappers from being created.
;; That's because `the-color-database` produces colors which, without caching, will keep creating
;; contracts from the same values.
(define (color/c #:immutable? [imm? #f])
  (cache/c
   (and/c
    (is-a?/c color%)
    (color-init/c imm?)
    (object/c
     [alpha (->m (real-in 0 1))]
     [red  (->m byte?)]
     [blue (->m byte?)]
     [green (->m byte?)]
     [copy-from (->m (is-a?/c color%) (is-a?/c color%))]
     [ok? (->m boolean?)]
     [set (->* (color-mutable? byte? byte? byte?)
               ((real-in 0 1))
               void?)]))))

(define color%/c
  (dependent-classof/c
   (λ args
     (define immutable?
       (match args
         [(list _) #t]
         [_ #f]))
     (color/c #:immutable? immutable?))))

(define make-color/c
  (->* (byte? byte? byte?)
       ((real-in 0 1))
       (color/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; point%

(define point%/c
  (class/c
    (get-x (->m real?))
    (get-y (->m real?))
    (set-x (->m real? void?))
    (set-y (->m real? void?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font%

(define font%/c
  (class/c
    (get-face (->m (or/c string? #f)))
    (get-family (->m font-family/c))
    (get-feature-settings (->m font-feature-settings/c))
    (get-font-id (->m exact-integer?))
    (get-hinting (->m font-hinting/c))
    (get-point-size (->m (integer-in 1 1024)))
    (get-size (->*m [] [any/c] (real-in 0.0 1024.0)))
    (get-size-in-pixels (->m boolean?))
    (get-smoothing (->m font-smoothing/c))
    (get-style (->m font-style/c))
    (get-underlined (->m boolean?))
    (get-weight (->m font-weight/c))
    (screen-glyph-exists? (->*m (char?) (any/c) boolean?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pen-list%

(define pen-list%/c
  (class/c
    [find-or-create-pen
      (->*m ((or/c (is-a?/c color%) string?)
             real?
             pen-style/c)
            (pen-cap-style/c
             pen-join-style/c)
            (or/c (pen/c #:installed (set 'pen-list)) #f))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brush-list%

(define brush-list%/c
  (class/c
    (find-or-create-brush
      (->m (or/c (is-a?/c color%) string?)
           brush-style/c
           (or/c #f (brush/c #:installed (set 'brush-list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gradient%

(define linear-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [x1 real?]
      [y1 real?]
      [stops (listof (list/c real? (is-a?/c color%)))])
    [get-line (->m (values real? real? real? real?))]
    [get-stops (->m (listof (list/c real? (is-a?/c color%))))]))

(define radial-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [r0 real?]
      [x1 real?]
      [y1 real?]
      [r1 real?]
      [stops (listof (list/c real? (is-a?/c color%)))])
    [get-circles (->m (values real? real? real? real? real? real?))]
    [get-stops (->m (listof (list/c real? (is-a?/c color%))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitmap-dc%

(define ANY-STATUS-RE (re (star _)))

(define bitmap-dc%/c
  (dependent-class-object/c
   (class/c
    (init [bitmap (or/c #f (is-a?/c bitmap%))])
    [draw-bitmap-section-smooth
     (->* (dc-installed?
           (is-a?/c bitmap%)
           real? real?
           (and/c real? (not/c negative?))
           (and/c real? (not/c negative?))
           real? real?
           (and/c real? (not/c negative?))
           (and/c real? (not/c negative?)))
          ((or/c 'solid 'opaque 'xor)
           (or/c (is-a?/c color%) #f)
           (or/c (is-a?/c bitmap%) #f))
          boolean?)]
    [get-argb-pixels
     (->* (dc-installed?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           (and/c bytes? (not/c immutable?)))
          (any/c any/c)
          void?)]
    [get-bitmap (->m (or/c (is-a?/c bitmap%) #f))]
    [get-pixel (-> dc-installed?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer?
                   (is-a?/c color%)
                   boolean?)]
    [set-argb-pixels
     (->* (dc-installed?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           exact-nonnegative-integer?
           bytes?)
          (any/c any/c)
          void?)]
    [set-bitmap
     (->d ([self (and/c
                  (self/c
                   (λ (self)
                     (elementof/c
                      (or/c #f (bitmap-uninstall/c self))
                      (λ (self) (send self get-bitmap)))))
                  (dc-install/c bmp))]
           [bmp (or/c #f (bitmap-install-exclusive/c self))])
          [res void?])]
    [set-pixel (-> dc-installed? real? real? (is-a?/c color%) void?)])

   (λ (#:bitmap [bmp #f])
     (dc/c #:bitmap bmp
           #:re ANY-STATUS-RE))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; post-script-dc%

(define EPS-STATUS-RE
  (re (seq/close 'start-doc 'start-page (star 'draw) 'end-page 'end-doc)))

(define post-script-dc%/c
  (dependent-class-object/c
   (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]))
   (make-keyword-procedure
    (λ (kws kw-args)
      (define ht (make-immutable-hash (map cons kws kw-args)))
      (if (hash-ref ht '#:as-eps #f)
          (dc/c #:re EPS-STATUS-RE)
          (dc/c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdf-dc%

(define pdf-dc%/c
  (class-object/c
   (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]))
   (dc/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; svg-dc%

(define svg-dc%/c
  (class-object/c
   (class/c
    (init [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]
          [exists (or/c 'error 'append 'update 'can-update
                        'replace 'truncate
                        'must-truncate 'truncate/replace)]))
   (dc/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record-dc%

(define record-dc%/c
  (class-object/c
   (class/c
    (init [width (>=/c 0)]
          [height (>=/c 0)])
    [get-recorded-datum (->m any/c)]
    [get-recorded-procedure (->m ((is-a?/c dc<%>) . -> . void?))])
   (dc/c #:re ANY-STATUS-RE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; region%

(define region%/c
  (dependent-class-object/c
   (class/c
    (init [dc (or/c (is-a?/c dc<%>) #f)]))
   (λ (#:dc dc)
     (object/c
      (get-bounding-box (->m (values real? real? real? real?)))
      (get-dc (->m (is-a?/c dc<%>)))
      (in-region? (->m real? real? boolean?))
      (intersect (->m (is-a?/c region%) void?))
      ;; TODO: case-> should have better error message
      (is-empty? (if dc (->m boolean?) (case->)))
      (set-arc (->m real?
                    real?
                    (and/c real? (not/c negative?))
                    (and/c real? (not/c negative?))
                    real?
                    real?
                    void?))
      (set-ellipse (->m real?
                        real?
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        void?))
      (set-path (->*m ((is-a?/c dc-path%))
                      (real?
                       real?
                       (or/c 'odd-even 'winding))
                      void?))
      (set-polygon (->*m ((or/c (listof (is-a?/c point%))
                                (listof (cons/c real? real?))))
                         (real?
                          real?
                          (or/c 'odd-even 'winding))
                         void?))
      (set-rectangle (->m real?
                          real?
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          void?))
      (set-rounded-rectangle (->*m (real?
                                    real?
                                    (and/c real? (not/c negative?))
                                    (and/c real? (not/c negative?)))
                                   (real?)
                                   void?))
      (subtract (->m (is-a?/c region%) void?))
      (union (->m (is-a?/c region%) void?))
      (xor (->m (is-a?/c region%) void?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dc-path%

(define dc-path-open-key (make-attribute))
(define dc-path-open/c (attribute-update/c dc-path-open-key always-true))
(define dc-path-close/c (attribute-update/c dc-path-open-key (λ (_) #f)))

(define-modifiable
  #:level base
  (define dc-path-open? any/c)
  (define (dc-path-append/c _) any/c)

  #:level noop
  (define dc-path-open? (attribute-satisfies/c dc-path-open-key always-true))
  (define (dc-path-append/c path) any/c)

  #:level check
  (define dc-path-open? (attribute-satisfies/c dc-path-open-key id))
  (define (dc-path-append/c path)
    (attribute-update/c
     dc-path-open-key
     (λ (self-open?)
       (or self-open? (dc-path-open? path))))))

(define (dc-path/c #:open? [open? #f])
  (and/c
   (is-a?/c dc-path%)
   (attribute/c object? dc-path-open-key open?)
   (object/c
    [append (->i ([self (path) (dc-path-append/c path)]
                  [path (is-a?/c dc-path%)])
                 [res void?])]
    [arc (->* (dc-path-open/c real? real? real? real? real? real?)
              (any/c)
              void?)]
    [close (-> (and/c dc-path-open? dc-path-close/c) void?)]
    [curve-to (-> dc-path-open? real? real? real? real? real? real? void?)]
    [ellipse (-> dc-path-close/c
                 real?
                 real?
                 (and/c real? (not/c negative?))
                 (and/c real? (not/c negative?))
                 void?)]
    [get-bounding-box (->m (values real? real? real? real?))]
    [line-to (-> dc-path-open? real? real? void?)]
    [lines (->* (dc-path-open?
                 (or/c (listof (is-a?/c point%))
                       (listof (cons/c real? real?))))
                (real? real?)
                void?)]
    [move-to (-> dc-path-open/c real? real? void?)]
    [open? (->m boolean?)]
    [rectangle (-> dc-path-close/c
                   real?
                   real?
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   void?)]
    [reset (-> dc-path-close/c void?)]
    [reverse (->m void?)]
    [rotate (->m real? void?)]
    [rounded-rectangle (->* (dc-path-close/c
                             real?
                             real?
                             (and/c real? (not/c negative?))
                             (and/c real? (not/c negative?)))
                            (real?)
                            void?)]
    [scale (->m real? real? void?)]
    [text-outline (->* (dc-path-close/c (is-a?/c font%) string? real? real?)
                       (any/c)
                       void?)]
    [transform (->m (vector/c real? real? real? real? real? real?)
                    void?)]
    [translate (->m real? real? void?)])))

(define dc-path%/c (classof/c (dc-path/c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gl-config%

(define gl-config%/c
  (class/c
    (get-accum-size (->m (integer-in 0 256)))
    (get-depth-size (->m (integer-in 0 256)))
    (get-double-buffered (->m boolean?))
    (get-multisample-size (->m (integer-in 0 256)))
    (get-stencil-size (->m (integer-in 0 256)))
    (get-stereo (->m boolean?))
    (set-accum-size (->m (integer-in 0 256) void?))
    (set-depth-size (->m (integer-in 0 256) void?))
    (set-double-buffered (->m any/c void?))
    (set-multisample-size (->m (integer-in 0 256) void?))
    (set-stencil-size (->m (integer-in 0 256) void?))
    (set-stereo (->m any/c void?))
    (set-share-context (->m (or/c (is-a?/c gl-context%) #f) void?))
    (get-legacy? (->m boolean?))
    (set-legacy? (->m any/c void?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-database<%>

(define color-database<%>
  (interface ()
    [find-color (->m string? (or/c (color/c #:immutable? #t) #f))]
    [get-names (->m (listof string?))]))

;; TODO: Ideally we wouldn't need this since the global color database would
;; actually use color-database<%>
(define color-database/c
  (object/c
    [find-color (->m string? (or/c (color/c #:immutable? #t) #f))]
    [get-names (->m (listof string?))]))
