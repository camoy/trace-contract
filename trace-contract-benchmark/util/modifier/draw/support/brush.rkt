#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide brush-style/c
         transformation-vector/c

         brush-install/c
         brush-uninstall/c

         brush/c
         brush%/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         contract-etc
         racket/class
         racket/draw/private/bitmap
         racket/draw/private/brush
         racket/draw/private/color
         racket/function
         racket/set
         trace-contract/benchmarks/util/measure
         "bitmap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(define brush-style/c
  (or/c 'transparent 'solid 'opaque
        'xor 'hilite 'panel
        'bdiagonal-hatch 'crossdiag-hatch
        'fdiagonal-hatch 'cross-hatch
        'horizontal-hatch 'vertical-hatch))

(define transformation-vector/c
  (vector/c (vector/c real? real? real? real? real? real?)
            real? real? real? real? real?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attribute

(define brush-installed-key (make-attribute))

(define-modifiable
  #:level base
  (define (brush-init/c _) any/c)
  (define brush-bypass/c has-contract?)
  (define (brush-install/c _) any/c)
  (define (brush-uninstall/c _) any/c)
  (define brush-uninstalled? any/c)

  #:level noop
  (define (brush-init/c s)
    (attribute/c object? brush-installed-key s))
  (define brush-bypass/c
    (attribute-present/c brush-installed-key))
  (define (brush-install/c x)
    (attribute-update/c brush-installed-key id))
  (define (brush-uninstall/c x)
    (or/c (not/c (attribute-present/c brush-installed-key))
          (attribute-update/c brush-installed-key id)))
  (define brush-uninstalled?
    (attribute-satisfies/c brush-installed-key always-true))

  #:level check
  (define (brush-init/c s)
    (attribute/c object? brush-installed-key s))
  (define brush-bypass/c
    (attribute-present/c brush-installed-key))
  (define (brush-install/c x)
    (attribute-update/c brush-installed-key (λ (s) (set-add s x))))
  (define (brush-uninstall/c x)
    (or/c (not/c (attribute-present/c brush-installed-key))
          (attribute-update/c brush-installed-key (λ (s) (set-remove s x)))))
  (define brush-uninstalled?
    (attribute-satisfies/c brush-installed-key (λ (s) (set-empty? s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brush/c

(define (brush/c #:installed [s (set)])
  (or/c
   brush-bypass/c
   (and/c
    (is-a?/c brush%)
    (brush-init/c s)
    (object/c
     [get-color (->m (is-a?/c color%))]
     [get-stipple (->m (or/c (is-a?/c bitmap%) #f))]
     [get-style (->m brush-style/c)]
     [set-color (case->
                 (-> brush-uninstalled? (or/c (is-a?/c color%) string?) void?)
                 (-> brush-uninstalled? byte? byte? byte? void?))]
     [set-stipple
      (->d ([self (and/c
                   brush-uninstalled?
                   (self/c
                    (λ (self)
                      (elementof/c
                       (or/c #f (bitmap-uninstall/c self))
                       (λ (self) (send self get-stipple))))))]
            [bmp (or/c #f (bitmap-install/c self))])
           ([vec (or/c transformation-vector/c #f)])
           [res void?])]
     [set-style (-> brush-uninstalled? brush-style/c void?)]))))

(define brush%/c (classof/c (brush/c)))
