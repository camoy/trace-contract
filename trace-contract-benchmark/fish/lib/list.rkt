#lang racket

(provide
 #; {[Listof X] -> Boolean}
 distinct?

 #; {[NEListof X] -> [NEListof X]}
 rotate)

;; -----------------------------------------------------------------------------

(define (rotate l)
  (match l
    [`(,fst ,snd ...) (append snd (list fst))]))

(define distinct?
  (flat-named-contract
   "distinct"
   (lambda (names)
     (let loop ([six names])
       (if (empty? six)
           #t
           (and (not (member (first six) (rest six))) (loop (rest six))))))))
