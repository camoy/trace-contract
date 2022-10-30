#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/undefined
                    racket/class
                    racket/contract
                    trace-contract]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/contract
               trace-contract)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Trace Contracts}
@author{Cameron Moy}

@defmodule[trace-contract]

@margin-note{
  This package has not been officially released.
  Backwards compatibility is not guaranteed.
}

An ordinary contract is memoryless.
When a flat contract blesses a value,
it can't retain any information
about that value
for use later.
This precludes monitoring any
temporal or multi-call properties.

A trace contract,
by contrast,
has memory.
It collects values that
flow through contract interposition points
and accumulates them into
a piece of user-defined state.
This state decides whether
a contract violation occurred or not.

For example,
here is a contract that ensures
the sequence of inputs to a function
increases over time:

@examples[#:eval evaluator #:no-result
  (define (gets-bigger-fold acc cur)
    (if (> cur acc) cur (fail)))

  (define gets-bigger/c
    (trace/c ([x real?])
      (-> x any)
      (accumulate -inf.0
        [(x) gets-bigger-fold])))]

The @racket[gets-bigger/c] trace contract
collects function input values
and supplies them to @racket[gets-bigger-fold].
Initially,
the accumulator is @racket[-inf.0].
If the input is ever smaller
than the accumulator,
@racket[get-bigger-fold]
indicates a contract error by returning
a failure accumulator.
Otherwise,
the input becomes the next accumulator.

Here's an interaction that uses this contract:

@examples[#:eval evaluator #:label #f
  (define/contract (add42 x)
    gets-bigger/c
    (+ x 42))

  (add42 1)
  (add42 2)
  (eval:error (add42 1))]

@section{Basics}

@defform[
  (trace/c ([id contract-expr] ...+)
    maybe-option
    inner-contracts
    clause ...+)
  #:grammar
  [(maybe-option (code:line)
                 (code:line #:global))
   (inner-contracts (code:line contract-expr)
                    (code:line (@#,racket[values] contract-expr ...+)))
   (clause (code:line (@#,racket[accumulate] init-expr [(id ...+) expr] ...+))
           (code:line (@#,racket[combine] clause ...))
           (code:line (clause-macro e ...)))]]{
  Binds each @racket[id] to a collector contract
  and returns the inner contracts.
  When a value flows through a collector contract,
  clauses dependent on that collector will be
  notified and may raise a contract error.

  The @racket[#:global] option initializes the state of the
  accumulators at definition time.
  Hence,
  they are initialized only once.
  By default,
  accumulators are initialized at each
  trace-contract attachment.

  @defsubform[(accumulate init-expr [(id ...+) folder-expr] ...+)]{
    The initial accumulator is @racket[init-expr].
    Each subclause comes with a sequence of @racket[id]s,
    specifying dependent collectors,
    and a folder function.
    Once every dependent collector for a subclause
    has collected a new value,
    the folding function is called with the accumulator
    and all of these values.
    If the return value is a @racket[fail] value,
    then a contract violation is raised.
    Otherwise,
    the return value is the new accumulator.

    Suppose a subclause has two dependencies:
    @racket[x] and @racket[y].
    Consider the situation where
    @racket[x] collects @racket[5],
    then @racket[x] collects @racket[13],
    then @racket[y] collects @racket[19].
    In this case,
    the subclause is only triggered once with
    @racket[13] and @racket[19];
    the first value collected by @racket[x],
    @racket[5],
    is overwritten.

    If the folder function has a
    mandatory @racket[#:blame] keyword argument,
    then the blame object is supplied.
    This information is necessary
    for custom error messages.
  }

  @defsubform[(combine clause ...)]{
    The union of the @racket[clause]s.
    This is most useful for user-defined macros;
    see @racket[trace-clause-macro].
  }
}

@defproc[(fail [#:reset reset any/c init-expr]
               [#:explain explain (-> any) (λ () ....)])
               fail?]{
  Returns a value that
  indicates a contract failure
  when used as an accumulator.

  The @racket[#:reset] value is used as the accumulator
  after a violation has occurred.
  This is only meaningful if
  the contract error was caught
  by an exception handler.
  By default,
  it is the initial accumulator
  of the clause.

  The @racket[#:explain] option is called
  to provide a custom error
  in the case of a contract violation.
  While @racket[raise-blame-error]
  could be called directly,
  doing does not reset
  the accumulator appropriately.
}

@defproc[(trace-contract? [v any/c]) boolean?]{
  Returns if @racket[v] is a trace contract.
}

@defproc[(fail? [v any/c]) boolean?]{
  Returns if @racket[v] is a failure value.
}

@section{Collector Contracts}

@defproc[(list/t [v any/c] ...) collector-contract?]{
  Given a values @racket[v] that include exactly one collector contract,
  returns a new collector that wraps the old one
  and constructs a list around an element before adding it
  to the trace.
}

@defproc[(map/t [f (-> any/c any/c)] [v any/c] ...) collector-contract?]{
  Given a function @racket[f] and values @racket[v]
  that include exactly one collector contract,
  returns a new collector that wraps the old one
  and applies @racket[f] before adding an element to the trace.
}

@defproc[(collector-contract? [v any/c]) boolean?]{
  Returns if @racket[v] is a collector contract.
}

@section{Syntax}

These bindings are provided at phase 1.

@defproc[(trace-clause-macro [transformer (-> syntax? syntax?)])
         trace-clause-macro?]{
  Returns a syntax transformer that,
  when bound by @racket[define-syntax],
  can be used as a trace clause.
  The provided procedure
  is expected to return a valid
  @racket[trace/c] clause
  as syntax.
}

@defproc[(trace-clause-expand [stx syntax?]) syntax?]{
  Expands the given trace clause
  into core form clauses
  (i.e. @racket[accumulate] and @racket[combine]).
}

@defproc[(trace-clause-macro? [v any/c]) boolean?]{
  Returns if @racket[v] is a trace clause macro.
}

@section{Full Clause}

@defform[(full (id ...+) expr)]{
  A trace clause that
  stores the entire history of values
  for each collector in a @seclink["streams" #:doc '(lib "scribblings/reference/reference.scrbl")]{stream}
  known as a trace.
  The @racket[expr] must evaluate to a predicate
  that will be applied to all of the traces
  (one for each @racket[id])
  whenever any of them change.
  If the function returns @racket[#f],
  a contract violation is raised.
}

@defproc[(trace-merge [t trace?] ...) trace?]{
  Returns a new trace that is the
  time-ordered merge of input traces @racket[t].
}

@defproc[(trace? [v any/c]) boolean?]{
  Returns if @racket[v] is a trace.
}

@section{Track Clause}

@defform[(track sus-expr clause ...+)]{
  This form acts like @racket[combine]
  except that it tracks contract parties and
  causes the default error message to
  report suspects according to @racket[sus-expr].
  The @racket[sus-expr] must be an object that
  implements @racket[suspect<%>].
}

@defthing[#:kind "interface" suspect<%> interface?]{
  This interface describes objects that
  track and report suspects.
  It includes two methods:
  @itemlist[
  @item{@racket[add] ---
    Given a blame party,
    returns a new instance of @racket[suspect<%>]
    that incorporates this party.}
  @item{@racket[value] ---
    Returns a datum
    that is printed
    as the suspect information.}
  ]
}

@defthing[setof-suspect (is-a?/c suspect<%>)]{
  Reports an unordered set of suspects.
}

@defthing[listof-suspect (is-a?/c suspect<%>)]{
  Reports a time-ordered list of suspects---one
  for each collected value.
}
