#lang racket

(provide toint)

#; {Real -> Int}
(define (toint x)
  (inexact->exact (round x)))