#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide pen-cap-style/c
         pen-join-style/c
         pen-style/c

         pen-install/c
         pen-uninstall/c
         pen-uninstalled?

         pen/c
         pen%/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         contract-etc
         racket/class
         racket/contract
         racket/function
         racket/draw/private/bitmap
         racket/draw/private/color
         racket/draw/private/pen
         racket/set
         trace-contract/benchmarks/util/measure
         "bitmap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(define pen-cap-style/c
  (or/c 'round 'projecting 'butt))

(define pen-join-style/c
  (or/c 'round 'bevel 'miter))

(define pen-style/c
  (or/c 'transparent 'solid 'xor 'hilite
        'dot 'long-dash 'short-dash 'dot-dash
        'xor-dot 'xor-long-dash 'xor-short-dash
        'xor-dot-dash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attribute

(define pen-installed-key (make-attribute))

(define-modifiable
  #:level base
  (define (pen-init/c _) any/c)
  (define pen-bypass/c has-contract?)
  (define (pen-install/c _) any/c)
  (define (pen-uninstall/c _) any/c)
  (define pen-uninstalled? any/c)

  #:level noop
  (define (pen-init/c s)
    (attribute/c object? pen-installed-key s))
  (define pen-bypass/c
    (attribute-present/c pen-installed-key))
  (define (pen-install/c x)
    (attribute-update/c pen-installed-key id))
  (define (pen-uninstall/c x)
    (or/c (not/c (attribute-present/c pen-installed-key))
          (attribute-update/c pen-installed-key id)))
  (define pen-uninstalled?
    (attribute-satisfies/c pen-installed-key always-true))

  #:level check
  (define (pen-init/c s)
    (attribute/c object? pen-installed-key s))
  (define pen-bypass/c
    (attribute-present/c pen-installed-key))
  (define (pen-install/c x)
    (attribute-update/c pen-installed-key (λ (s) (set-add s x))))
  (define (pen-uninstall/c x)
    (or/c (not/c (attribute-present/c pen-installed-key))
          (attribute-update/c pen-installed-key (λ (s) (set-remove s x)))))
  (define pen-uninstalled?
    (attribute-satisfies/c pen-installed-key (λ (s) (set-empty? s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pen%

(define (pen/c #:installed [s (set)])
  ;; This is slightly awkward, but we need it for `get-pen` to acquire
  ;; an attributed field in the case of an internally-created pen.
  (or/c
   pen-bypass/c
   (and/c
    (is-a?/c pen%)
    (pen-init/c s)
    (object/c
     [get-cap (->m pen-cap-style/c)]
     [get-color (->m (is-a?/c color%))]
     [get-join (->m pen-join-style/c)]
     [get-stipple (->m (or/c (is-a?/c bitmap%) #f))]
     [get-style (->m pen-style/c)]
     [get-width (->m (real-in 0 255))]
     [set-cap (-> pen-uninstalled? pen-cap-style/c void?)]
     [set-color (case->
                 (-> pen-uninstalled? (or/c (is-a?/c color%) string?) void?)
                 (-> pen-uninstalled? byte? byte? byte? void?))]
     [set-join (-> pen-uninstalled? pen-join-style/c void?)]
     ;; TODO: almost verbatim set-bitmap
     [set-stipple (->d ([self
                         (self/c
                          (λ (self)
                            (and/c
                             pen-uninstalled?
                             (elementof/c
                              (or/c #f (bitmap-uninstall/c self))
                              (λ (self) (send self get-stipple))))))]
                        [bmp (or/c #f (bitmap-install/c self))])
                       [res void?])]
     [set-style (-> pen-uninstalled? pen-style/c void?)]
     [set-width (-> pen-uninstalled? (real-in 0 255) void?)]))))

(define pen%/c (classof/c (pen/c)))
