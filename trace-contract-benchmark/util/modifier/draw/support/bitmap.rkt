#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide bitmap-kind/c

         bitmap-install/c
         bitmap-install-exclusive/c
         bitmap-uninstall/c

         bitmap/c
         bitmap%/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require trace-contract
         contract-etc
         racket/class
         racket/contract
         racket/draw/private/bitmap
         racket/draw/private/color
         racket/function
         racket/match
         racket/set
         trace-contract/benchmarks/util/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(define bitmap-kind/c
  (or/c 'unknown 'unknown/mask 'unknown/alpha
        'gif 'gif/mask 'gif/alpha
        'jpeg 'jpeg/alpha
        'png 'png/mask 'png/alpha
        'xbm 'xbm/alpha 'xpm 'xpm/alpha
        'bmp 'bmp/alpha))

(define BITMAP-UNLOADABLES
  (set 'make-platform-bitmap
       'make-screen-bitmap
       'make-bitmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attribute

(define bitmap-installed-key (make-attribute))
(define bitmap-save-data-key (make-attribute))
(define bitmap-origin-key (make-attribute))

(define-modifiable
  #:level base
  (define (bitmap-init/c save? origin installed)
    any/c)
  (define (bitmap-install/c _) any/c)
  (define (bitmap-install-exclusive/c _) any/c)
  (define (bitmap-uninstall/c _) any/c)
  (define bitmap-save-data? any/c)
  (define bitmap-loadable? any/c)

  #:level noop
  (define (bitmap-init/c save? origin installed)
    (attribute/c object?
                 bitmap-save-data-key save?
                 bitmap-origin-key origin
                 bitmap-installed-key installed))
  (define (bitmap-install/c x)
    (attribute-update/c bitmap-installed-key id))
  (define (bitmap-install-exclusive/c dc)
    (and/c (attribute-satisfies/c bitmap-installed-key always-true)
           (attribute-update/c bitmap-installed-key (λ (_) (set dc)))))
  (define (bitmap-uninstall/c x)
    (attribute-update/c bitmap-installed-key id))
  (define bitmap-save-data? (attribute-satisfies/c bitmap-save-data-key always-true))
  (define bitmap-loadable? (attribute-satisfies/c bitmap-origin-key always-true))

  #:level check
  (define (bitmap-init/c save? origin installed)
    (attribute/c object?
                 bitmap-save-data-key save?
                 bitmap-origin-key origin
                 bitmap-installed-key installed))
  (define (bitmap-install/c x)
    (attribute-update/c bitmap-installed-key (λ (s) (set-add s x))))
  (define (bitmap-install-exclusive/c dc)
    (define (ok? cur)
      (or (set-empty? cur) (set=? cur (set dc))))
    (and/c (attribute-satisfies/c bitmap-installed-key ok?)
           (attribute-update/c bitmap-installed-key (λ (_) (set dc)))))
  (define (bitmap-uninstall/c x)
    (attribute-update/c bitmap-installed-key (λ (s) (set-remove s x))))
  (define bitmap-save-data?
    (attribute-satisfies/c bitmap-save-data-key id))
  (define bitmap-loadable?
    (attribute-satisfies/c
     bitmap-origin-key
     (λ (origin) (not (set-member? BITMAP-UNLOADABLES origin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitmap/c

(define (bitmap/c #:save? [save? #f]
                  #:origin [origin #f]
                  #:installed [installed (set)])
  (and/c
   (is-a?/c bitmap%)
   (bitmap-init/c save? origin installed)
   (object/c
    [get-argb-pixels (->*m
                      (exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       (and/c bytes? (not/c immutable?)))
                      (any/c any/c #:unscaled? any/c)
                      void?)]
    [get-backing-scale (->m (>/c 0.0))]
    [get-depth (->m exact-nonnegative-integer?)]
    [get-height (->m exact-nonnegative-integer?)]
    [get-loaded-mask (->m (or/c (is-a?/c bitmap%) #f))]
    [get-width (->m exact-nonnegative-integer?)]
    [get-data-from-file
     (->i ([self any/c])
          [res (self)
               (and (bitmap-save-data? self)
                    (send self ok?)
                    (vector/c bitmap-kind/c
                              ;; actually always immutable
                              (or/c (is-a?/c color%) #f)
                              (and/c bytes? immutable?)
                              #:immutable #t))])]
    [has-alpha-channel? (->m boolean?)]
    [is-color? (->m boolean?)]
    [load-file
     (->i ([self (save?)
                 (let ([save-val? (if (unsupplied-arg? save?) #f save?)])
                   (and/c bitmap-loadable?
                          (attribute-set/c bitmap-save-data-key save-val?)))]
           [in (or/c path-string? input-port?)])
          ([kind bitmap-kind/c]
           [bg-color (or/c (is-a?/c color%) #f)]
           [complain? any/c]
           #:save-data-from-file? [save? any/c])
          [res boolean?])]
    [ok? (->m boolean?)]
    [save-file (->*m ((or/c path-string? output-port?)
                      (or/c 'png 'jpeg 'xbm 'xpm 'bmp))
                     ((integer-in 0 100)
                      #:unscaled? any/c)
                     boolean?)]
    [set-argb-pixels (->*m
                      (exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       bytes?)
                      (any/c any/c #:unscaled? any/c)
                      void?)]
    [set-loaded-mask (->m (is-a?/c bitmap%) void?)])))

(define bitmap%/c
  (dependent-classof/c
   (λ args
     (define save-val?
       (match args
         [(list _ _ _ _ _ save?) save?]
         [_ #f]))
     (bitmap/c #:save? save-val?))))
