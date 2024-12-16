#lang racket

(provide
  ;; SYNTAX
  #; (def/mp name:id in:pattern out:pattern)
  ;; defines name to be a simple pattern match expander from in to out
  def/mp

  (for-syntax (all-from-out syntax/parse)))

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (def/mp name pat exp)
  (define-match-expander name (λ (stx) (syntax-parse stx [pat exp]))))

(module+ test
  ;; a simple yuse 

  (def/mp posn [_ row column] #'[list (? natural? row) (? natural? column)])

  (match '[0 0]
    [(posn a b) (+ a b)]))