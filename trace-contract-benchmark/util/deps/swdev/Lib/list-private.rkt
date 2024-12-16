#lang racket

(provide

 #; {[Listof X] Y -> (Cons X ... Y)}
 snoc
 
 #; {[Cons X ... Y] -> [Listof X]}
 rdc
 
 #; {[NEListof X] -> [NEListof X]} 
 list-rotate

 list-rotate+
 list-rotate-

 #; {[Cons X [Listof X]] -> (values [Listof X] X)}
 split-off-last

 #; {[Listof X] N -> [Listof [Listof X]]}
 split-list)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof X] -> [NEListof X]}
(define (list-rotate lox)
  (snoc (rest lox) (first lox)))

(define list-rotate+ list-rotate)

(define (list-rotate- lox)
  (cons (last lox) (rdc lox)))
  
(define (snoc l x) (append l (list x)))

(define (rdc l)
  (if (not (pair? l))
      (raise-argument-error 'rdc "(and/c list? (not/c empty?))" l)
      (let loop ([l l] [x (cdr l)])
        (if (pair? x)
            (cons (car l) (loop x (cdr x)))
            '()))))

(define (split-off-last l)
  (define lst (gensym))
  (if (not (pair? l))
      (raise-argument-error 'split-off-last "(and/c list? (not/c empty?))" l)
      (values 
       (let loop ([l l] [x (cdr l)])
         (if (pair? x)
             (cons (car l) (loop x (cdr x)))
             (begin (set! lst (car l)) '())))
       lst)))

#; {[Listof X] N -> [Listof [Listof X]]}
(define (split-list tiles0 rows#)
  (let L ([t* tiles0])
    (cond
      [(empty? t*) '()]
      [else
       (define row1 (take t* rows#))
       (cons row1 (L (drop t* rows#)))])))


;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (list-rotate- '[a b c]) '[c a b])
  (check-equal? (list-rotate+ '[a b c])  '[b c a])
  (check-equal? (rdc '(a b c)) '(a b))
  (check-equal? (let-values (([all-but last] (split-off-last '(a b c)))) `(,all-but ,last))
                '((a b) c)))