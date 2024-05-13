#lang racket/base

;; temporarily disable

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in racket/gui/base)
         racket/class
         racket/draw
         "util/test.rkt")

(enable-contracts!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests

(module+ test
  (require chk)

  (chk
   ;; (1) bitmap `get-data-from-file`
   #:do
   (define bmp1 (read-bitmap "support/x.png" 'png #:save-data-from-file? #f))
   (send bmp1 get-data-from-file) #f
   #:do (send bmp1 load-file "support/x.png" #:save-data-from-file? #f)
   (send bmp1 get-data-from-file) #f
   #:do (send bmp1 load-file "support/x.png" #:save-data-from-file? #t)
   #:? vector? (send bmp1 get-data-from-file)
   #:do (send bmp1 load-file "support/x.png" 'gif #:save-data-from-file? #t)
   (send bmp1 get-data-from-file) #f

   #:do (define bmp2 (make-object bitmap% 1 1))
   (send bmp2 get-data-from-file) #f
   #:do (send bmp2 load-file "support/x.png" #:save-data-from-file? #t)
   #:? vector? (send bmp2 get-data-from-file)

   #:do (define bmp3 (make-object bitmap% "support/x.png" 'png))
   (send bmp3 get-data-from-file) #f

   #:do (define bmp4 (make-object bitmap% "support/x.png" 'png #f #f 1.0 #t))
   #:? vector? (send bmp4 get-data-from-file)

   #:do (define bmp5 (make-bitmap 1 1))
   #:do (send bmp5 load-file "support/x.png" #:save-data-from-file? #t)
   #:? vector? (send bmp5 get-data-from-file)

   ;; (2) bitmap `load-file`
   ;; TODO: Add constraint when doing `racket/gui` to `make-screen-bitmap`
   ;; and `make-bitmap` in canvas%.
   #:do (define bmp6 (make-platform-bitmap 1 1))
   #:x (send bmp6 load-file "support/x.png")
   "does not satisfy"

   ;; (3) bitmap-dc `get-text-extent`, `get-char-height`, `get-char-width`
   ;; TODO: Really `get-bitmap` should be included in this list. Also in the
   ;; current implementation `get-argb-pixels` can be called even though it
   ;; really shouldn't.
   #:do (define bdc1 (new bitmap-dc%))
   #:? list? (call-with-values (λ () (send bdc1 get-text-extent "hi")) list)
   #:t (send bdc1 get-char-height)
   #:t (send bdc1 get-char-width)
   #:! #:t (send bdc1 get-bitmap)
   #:x (send bdc1 get-font) "does not satisfy"
   #:x (send bdc1 set-pixel 0 0 (make-object color%)) "does not satisfy"
   #:do (send bdc1 set-bitmap bmp3)
   #:t (send bdc1 get-font)
   #:do (send bdc1 set-bitmap #f)
   #:x (send bdc1 get-font) "does not satisfy"

   #:do (define bdc2 (new bitmap-dc% [bitmap bmp3]))
   #:t (send bdc2 get-font)
   #:do (send bdc2 set-bitmap #f)
   #:x (send bdc2 get-font) "does not satisfy"

   ;; (4) bitmap-dc `set-argb-pixels`
   ;; TODO: Add constraint when doing `racket/gui` to `make-screen-bitmap`
   ;; and `make-bitmap` in canvas%.

   ;; (5) bitmap-dc `set-bitmap`
   ;; TODO: control label cannot be set
   ;; TODO: order of the set (prohibit `bitmap-install` is anything in set
   ;; is a bitmap dc?)
   #:do (define bmp7 (make-bitmap 1 1))
   #:do (define bdc3 (new bitmap-dc% [bitmap bmp7]))
   #:do (define bdc4 (new bitmap-dc% [bitmap #f]))
   #:do (send bdc3 set-bitmap bmp7)
   #:x (send bdc4 set-bitmap bmp7) "does not satisfy"
   #:do (send bdc3 set-bitmap #f)
   #:do (send bdc4 set-bitmap bmp7)
   #:do (send bdc4 set-bitmap #f)

   #:do (define b1 (new brush%))
   #:do (send b1 set-stipple bmp7)
   #:x (send bdc4 set-bitmap bmp7) "does not satisfy"
   #:do (send b1 set-stipple #f)
   #:do (send bdc4 set-bitmap bmp7)
   #:do (send bdc4 set-bitmap #f)

   #:do (define p0 (new pen%))
   #:do (send p0 set-stipple bmp7)
   #:x (send bdc4 set-bitmap bmp7) "does not satisfy"
   #:do (send p0 set-stipple #f)
   #:do (send bdc4 set-bitmap bmp7)

   ;; (6) brush modification
   ;; TODO: can we abstract elementof contracts to something more reusable
   ;; (used by both set-bitmap and set-brush)
   ;; TODO: `dc-contract` with double self is messy
   #:do (define br1 (new brush%))
   #:do (send br1 set-color "red")
   #:do (send bdc4 set-brush br1)
   #:x (send br1 set-color "red") "does not satisfy"
   #:do (send bdc4 set-brush "red" 'solid)
   #:do (send br1 set-color "red")

   ;; Make sure that internal brushes are tracked too.
   #:do (define rdc (new record-dc%))
   #:do (define ibr (send rdc get-brush))
   #:do (send rdc set-brush ibr)

   ;; (7) brush list modification
   #:do (define br2 (send the-brush-list find-or-create-brush "red" 'solid))
   #:x (send br2 set-color "blue") "does not satisfy"

   #:do (define brlist (new brush-list%))
   #:do (define br3 (send brlist find-or-create-brush "red" 'solid))
   #:x (send br3 set-color "blue") "does not satisfy"

   ;; (8) color immutable
   ;; This contract is preferable to what's currently there. At the moment,
   ;; new color databases cannot return immutable colors.
   #:do (define c1 (make-object color% 0 0 0))
   #:do (define c2 (make-object color% "red"))
   #:do (define c3 (send the-color-database find-color "blue"))

   #:t (send c1 set 1 1 1)
   #:x (send c2 set 1 1 1) "does not satisfy"
   #:x (send c3 set 1 1 1) "does not satisfy"

   ;; (9) dc document and page, start and end
   #:do (define (make-svg-dc)
          (new svg-dc% [width 1] [height 1] [output (open-output-string)]))
   #:do (define (make-ps-dc eps?)
          (new post-script-dc%
               [as-eps eps?]
               [interactive #f]
               [output (open-output-string)]))
   #:do (define (make-pdf-dc)
          (new pdf-dc%
               [interactive #f]
               [output (open-output-string)]))
   #:do (define (make-record-dc)
          (new record-dc%))

   #:do (define (single-page-sequence dc)
          (send dc start-doc "")
          (send dc start-page)
          (send dc draw-point 0 0)
          (send dc end-page)
          (send dc end-doc))
   #:t (single-page-sequence (make-svg-dc))
   #:t (single-page-sequence (make-ps-dc #f))
   #:t (single-page-sequence (make-pdf-dc))
   #:t (single-page-sequence (make-record-dc))

   #:do (define (multi-page-sequence dc)
          (send dc start-doc "")
          (send dc start-page)
          (send dc draw-point 0 0)
          (send dc end-page)
          (send dc start-page)
          (send dc draw-line 0 0 1 1)
          (send dc end-page)
          (send dc end-doc))
   #:t (multi-page-sequence (make-svg-dc))
   #:t (multi-page-sequence (make-ps-dc #f))
   ;; TODO: shouldn't this work?
   ;#:t (multi-page-sequence (make-pdf-dc))
   #:t (multi-page-sequence (make-record-dc))

   #:do (define (no-start-doc dc)
          (send dc draw-point 0 0))
   #:x (no-start-doc (make-svg-dc)) "does not satisfy"
   #:x (no-start-doc (make-ps-dc #f)) "does not satisfy"
   #:x (no-start-doc (make-pdf-dc)) "does not satisfy"

   #:do (define (no-start-page dc)
          (send dc start-doc "")
          (send dc draw-point 0 0))
   #:x (no-start-page (make-svg-dc)) "does not satisfy"
   #:x (no-start-page (make-ps-dc #f)) "does not satisfy"
   #:x (no-start-page (make-pdf-dc)) "does not satisfy"

   #:do (define (already-ended-page dc)
          (send dc start-doc "")
          (send dc start-page)
          (send dc end-page)
          (send dc draw-point 0 0))
   #:x (already-ended-page (make-svg-dc)) "does not satisfy"
   #:x (already-ended-page (make-ps-dc #f)) "does not satisfy"
   #:x (already-ended-page (make-pdf-dc)) "does not satisfy"

   #:do (define (double-start-doc dc)
          (send dc start-doc "")
          (send dc end-doc)
          (send dc start-doc ""))
   #:x (double-start-doc (make-svg-dc)) "does not satisfy"
   #:x (double-start-doc (make-ps-dc #f)) "does not satisfy"
   #:x (double-start-doc (make-pdf-dc)) "does not satisfy"

   ;; (10) dc-path open
   #:do (define dp1 (new dc-path%))

   #:x (send dp1 close) "does not satisfy"

   #:do (send dp1 move-to 0 0)
   #:do (send dp1 line-to 5 5)
   #:do (send dp1 ellipse 5 5 10 10)
   #:x (send dp1 line-to 10 10) "does not satisfy"

   #:do (define dp2 (new dc-path%))
   #:do (send dp2 append dp1)
   #:x (send dp1 line-to 10 10) "does not satisfy"
   #:do (send dp1 move-to 0 0)
   #:do (send dp2 append dp1)
   #:do (send dp2 line-to 5 5)

   ;; (11) pen modification
   #:do (define p1 (new pen%))
   #:do (send p1 set-color "red")
   #:do (send bdc4 set-pen p1)
   #:x (send p1 set-color "red") "does not satisfy"
   #:do (send bdc4 set-pen "red" 1 'solid)
   #:do (send p1 set-color "red")

   ;; Make sure that internal brushes are tracked too.
   #:do (define ip (send rdc get-pen))
   #:do (send rdc set-pen ip)

   ;; (12) pen list modification
   #:do (define p2 (send the-pen-list find-or-create-pen "red" 1 'solid))
   #:x (send p2 set-color "blue") "does not satisfy"

   #:do (define plist (new pen-list%))
   #:do (define p3 (send plist find-or-create-pen "red" 1 'solid))
   #:x (send p3 set-color "blue") "does not satisfy"

   ;; (13) post-script-dc
   #:t (single-page-sequence (make-ps-dc #t))
   #:x (multi-page-sequence (make-ps-dc #t)) "does not satisfy"

   ;; (14) region
   #:do (define rgn (new region% [dc #f]))
   #:x (send rgn is-empty?) "the expected number of arguments"

   ;; (15) record-dc
   #:t (no-start-doc (make-record-dc))
   #:t (no-start-page (make-record-dc))
   #:t (already-ended-page (make-record-dc))
   #:t (double-start-doc (make-record-dc))
   ))
|#
