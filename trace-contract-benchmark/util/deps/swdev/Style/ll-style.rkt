#lang at-exp racket

(provide ll-style)

@; ---------------------------------------------------------------------------------------------------
(require (only-in scribble/manual nested tabular tt) (only-in scribble/base hspace))
(require scribble/html-properties)
(require scribble/decode)

@; ---------------------------------------------------------------------------------------------------
#; { SYNTAX
@a-a{
 Q & A |
...
}}

(define (ll-style . qs-and-as)
  (define (x . s)
    (let* ([s (string-split (apply string-append s) "|\n")]
	   [s (map (λ (x) (regexp-replace* "\\\\\n" x "")) s)]
	   [s (map (λ (x) (map decode-string (string-split x "&"))) s)])
      s))
  (define cell-style
    (attributes
      '( (valign . "top")
	 (width . "280")
	 (style . "padding: 10px;"))))

  (define cells (apply x qs-and-as))
  @tabular[#:row-properties '(bottom-border)
	    #:cell-properties (for/list ((_ cells)) (list cell-style cell-style ))
	    #:style 'boxed
	    cells 
	    ])

@; ---------------------------------------------------------------------------------------------------
(module+ test

  @ll-style{
 What do you mean with ``transfer''? & I didn't know the word back then. |
 Regardless, what problem did you observe? & In '94 we started teaching \
 the design recipe in earnest. \
 And it turned out to be a beast 
 })
