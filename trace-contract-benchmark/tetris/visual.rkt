#lang racket/base

(provide
 world->image
 blocks->image
 block->image
 place-block)

(require racket/format
         pict
         "data.rkt"
         "consts.rkt"
         "world.rkt"
         "aux.rkt")

;; Visualize whirled peas
;; World -> Scene
(define (world->image w)
  (cc-superimpose
   (blank (* board-width block-size)
          (* board-height block-size))
   (blocks->image (append (tetra-blocks (world-tetra w))
                          (append (ghost-blocks w)
                                  (world-blocks w))))))

;; BSet -> Scene
(define (blocks->image bs)
  (foldr (λ (b img)
           (cond [(<= 0 (block-y b)) (place-block b img)]
                 [else img]))
         (blank (add1 (* board-width block-size))
                (add1 (* board-height block-size)))
         bs))

;; Visualizes a block.
;; Block -> Image
(define (block->image b)
  (filled-rectangle (add1 block-size)
                    (add1 block-size)
                    #:color (~a (block-color b))))

;; Block Scene -> Scene
(define (place-block b scene)
  (pin-over scene
            (* (block-x b) block-size)
            (* (block-y b) block-size)
            (block->image b)))
