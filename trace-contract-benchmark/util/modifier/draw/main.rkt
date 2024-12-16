#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/contract/base
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
         racket/draw/private/record-dc
         racket/draw/private/post-script-dc
         racket/draw/private/svg-dc
         racket/draw/private/ps-setup
         racket/draw/private/gl-config
         racket/draw/private/gl-context
         "support/main.rkt"
         "support/dc-intf.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncontracted exports

(provide color-database<%>
         font-list% the-font-list make-font
         font-name-directory<%> the-font-name-directory
         dc<%>
         recorded-datum->procedure
         ps-setup% current-ps-setup
         get-face-list
         get-family-builtin-face
         gl-context<%>
         get-current-gl-context

         ;; predicates/contracts
         brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         font-smoothing/c
         font-hinting/c
         font-feature-settings/c)

(define dc-checked-mixin (mixin () (dc-checked<%>) (super-new)))
(define checked-bitmap-dc% (dc-checked-mixin bitmap-dc%))
(define checked-svg-dc% (dc-checked-mixin svg-dc%))
(define checked-pdf-dc% (dc-checked-mixin pdf-dc%))
(define checked-post-script-dc% (dc-checked-mixin post-script-dc%))
(define checked-record-dc% (dc-checked-mixin record-dc%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracted exports

(provide
 (contract-out
  [the-color-database color-database/c]
  [current-font-list (parameter/c (or/c (is-a?/c font-list%) #f))]
  [the-pen-list (instanceof/c pen-list%/c)]
  [the-brush-list (instanceof/c brush-list%/c)]
  [make-bitmap (->* (exact-positive-integer?
                     exact-positive-integer?)
                    (any/c
                     #:backing-scale (>/c 0.0))
                    (bitmap/c))]
  [make-platform-bitmap (->* (exact-positive-integer?
                              exact-positive-integer?)
                             (#:backing-scale (>/c 0.0))
                             (bitmap/c #:origin 'make-platform-bitmap))]
  [make-monochrome-bitmap (->* (exact-positive-integer?
                                exact-positive-integer?)
                               ((or/c #f bytes?))
                               (bitmap/c))]
  [read-bitmap
   (->i ([in (or/c path-string? input-port?)])
        ([kind bitmap-kind/c]
         [bg-color (or/c (is-a?/c color%) #f)]
         [complain? any/c]
         #:save-data-from-file? [save? any/c]
         #:backing-scale [bs (>/c 0.0)]
         #:try-@2x? [2x? any/c])
        [res (save?) (bitmap/c #:save? save?)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class exports

(provide
 (contract-out
  [color%            color%/c]
  [point%            point%/c]
  [font%             font%/c]
  [pen%              pen%/c]
  [pen-list%         pen-list%/c]
  [brush%            brush%/c]
  [brush-list%       brush-list%/c]
  [rename checked-bitmap-dc%       bitmap-dc%       bitmap-dc%/c]
  [rename checked-post-script-dc%  post-script-dc%  post-script-dc%/c]
  [rename checked-pdf-dc%          pdf-dc%          pdf-dc%/c]
  [rename checked-svg-dc%          svg-dc%          svg-dc%/c]
  [rename checked-record-dc%       record-dc%       record-dc%/c]
  [linear-gradient%  linear-gradient%/c]
  [radial-gradient%  radial-gradient%/c]
  [region%           region%/c]
  [dc-path%          dc-path%/c]
  [gl-config%        gl-config%/c]
  [bitmap%           bitmap%/c]
  [make-color        make-color/c]
  [make-pen          make-pen/c]
  [make-brush        make-brush/c]))
