#lang racket/gui

(provide
 #; {Pict Pict Pict (Instanceof Color%) Natural -> Pict}
 ;; add a spline from dot1 to dot2 via the center of sq 
 pin-curve

 #;{Pict Pict -> Pict}
 ;; does the second pict occur in the first one?
 sub-pict?)

;; ---------------------------------------------------------------------------------------------------
(require pict)

;; ---------------------------------------------------------------------------------------------------
(define (pin-curve sq dot1 dot2 color size)
  #; { (Instanceof DC<%) Integer Integer -> Void }
  (define (draw-connections dc dx dy)
    (define old-pen (send dc get-pen))
    ;; --------------------------------------------------
    (define-values (x1 y1) (d+ sq dot1 cc-find dx dy))
    (define-values (x2 y2) (d+ sq dot2 cc-find dx dy))
    (define-values (xc yc) (d+ sq sq cc-find dx dy))
    (send dc set-pen (new pen% [width 3] [color color]))
    (send dc draw-spline x1 y1 xc yc x2 y2)
    ;; --------------------------------------------------
    (send dc set-pen old-pen))
  (define connections (dc draw-connections size size))
  (cc-superimpose sq connections))

#; { Pict Pict (Pict Pict -> (values Integer Integer)) Integer Integer -> (values Integer Integer)}
;; find sub-pict in pict via finder, then add dx and dy to each coordinate
(define (d+ pict sub-pict finder dx dy)
  (define-values (x y) (finder pict sub-pict))
  (values (+ dx x) (+ dy y)))

;; ---------------------------------------------------------------------------------------------------
(define ((sub-pict? p1) p2)
  (with-handlers ([exn:fail? (λ (x) (displayln x) #f)]) (define-values (x y) (lc-find p2 p1)) #t))