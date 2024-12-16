#lang racket

(provide
 (rename-out
   #; {Collection -> Boolean}
  [unique/c-set distinct?]
  [unique/c-set unique/c]))

(define (unique/c-set coll-of-x)
  (define list-of-x (for/list ([x coll-of-x]) x))
  (define set-of-x  (apply set list-of-x))
  (= (set-count set-of-x) (length list-of-x)))

;; ----------------------------------------------------------------------------------------
;; just playing

(define (unique/c-square coll-of-x #:equal (equal? equal?))
  (let loop ([list-of-x (for/list ([x coll-of-x]) x)])
    (cond
      [(empty? list-of-x) #true]
      [else
	(define x (first list-of-x))
	(and (not (member x (rest list-of-x) equal?)) (loop (rest list-of-x)))])))

(module+ test

  (define (stress unique n k)
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)

    (define yes-s (build-list n values))
    (define no-s  (append yes-s (list (first yes-s))))

    (time
     (for/and ([_ k])
       (and (unique yes-s) (not (unique no-s))))))

  '[10 3]
  (stress unique/c-square     10 3)
  (stress unique/c-set 10 3)

  '[10 300]
  (stress unique/c-square     10 300)
  (stress unique/c-set 10 300)

  '[100 3]
  (stress unique/c-square     100 3)
  (stress unique/c-set 100 3)

  '[100 300]
  (stress unique/c-square     100 300)
  (stress unique/c-set 100 300)

  '[10000 3]
  (stress unique/c-square     10000 30)
  (stress unique/c-set 10000 30))
    
  
