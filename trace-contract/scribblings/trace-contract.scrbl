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

@defform[(object-trace/c opt ... sig ...)
	 #:grammar
	 [(opt (code:line #:satisfies mach-expr)
               (code:line #:refutes mach-expr)
               (code:line #:include ctc-expr)
               (code:line #:extend ctc-expr))
	  (sig (code:line (@#,racket[->m] dom ... res) evt)
	       (code:line (@#,racket[->dm] [dom-id dom] ... [res-id res]) evt))]]{
  Constructs an object contract,
  similar to @racket[object/c],
  but with tracing of method events.
  The @racket[#:satisfies] and @racket[#:refutes] keywords
  should be given a machine to determine satisfaction
  (refutation) of the trace of method events.
  The @racket[#:include] and @racket[#:extend] keywords
  keywords allow the trace to receive (give) method events.
}

@defthing[this-contract contract?]{
  Returns the current contract in @racket[object-trace/c]
  for use in the @racket[#:include] and @racket[#:extend] options.
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

@section{Attribute Contracts}

An attribute contract uses
metadata associated with a value
to determine if it passes the contract.
Consider the following "box-like" structure:

@examples[#:eval evaluator #:no-result
  (struct container (elem) #:mutable)

  (define init-attr (make-attribute 'init))
  (define container/c (attribute/c container? init-attr #f))
  (define initialized-container? (attribute-satisfies/c init-attr values))
  (define initialize-container/c (attribute-set/c init-attr #t))]

When a value flows through a @racket[container/c] contract,
it attaches a piece of metadata associating the attribute
@racket[init-attr] to @racket[#f].
The flat contract @racket[initialized-container?]
reads this metadata and is satisfied if the value is @racket[#t].
The @racket[initialize-contract/c] contract
sets the attribute to be @racket[#t].

Here are a few functions that make use of these contracts:

@examples[#:eval evaluator #:no-result
  (define/contract (make-container)
    (-> container/c)
    (container #f))

  (define/contract (set-container! c v)
    (-> initialize-container/c any/c void?)
    (set-container-elem! c v))

  (define/contract (container-ref c)
    (-> initialized-container? any/c)
    (container-elem c))]

Here is a correct call sequence:

@examples[#:eval evaluator #:label #f
  (define c-good (make-container))
  (set-container! c-good 42)
  (container-ref c-good)]

Here is an incorrect call sequence,
where @racket[container-ref] is called
before the container is initialized:

@examples[#:eval evaluator #:label #f
  (define c-bad (make-container))
  (eval:error (container-ref c-bad))]

@defproc[(make-attribute [name symbol? '???]) attribute?]{
  Returns a fresh attribute.
}

@defproc[(attribute? [v any/c]) boolean?]{
  Returns if @racket[v] is an attribute.
}

@defproc[(attribute/c [ctc flat-contract? any/c]
                      [key attribute?] [val any/c] ... ...)
                      attribute-contract?]{
  Produces an attribute contract that associates
  the given attributes to the given values.
}

@defproc[(attribute-contract? [v any/c]) boolean?]{
  Returns if @racket[v] is an attribute contract.
}

@defproc[(attribute-present/c [attr attribute?]) flat-contract?]{
  Produces a contract that is satisfied if the protected value
  has the given attribute.
}

@defproc[(attribute-present? [attr attribute?] [e any/c]) boolean?]{
  Returns if @racket[e] has the given attribute.
}

@defproc[(attribute-set/c [attr attribute?] [v any/c]) flat-contract?]{
  Produces a contract that sets the attribute to @racket[v].
}

@defproc[(attribute-set! [attr attribute?] [e any/c] [v any/c]) void?]{
  Sets the attribute on @racket[e] to @racket[v].
}

@defproc[(attribute-update/c [attr attribute?] [f (-> any/c any/c)]) contract?]{
  Produces a contract that updates the attribute using @racket[f].
}

@defproc[(attribute-update! [attr attribute?] [e any/c] [f (-> any/c any/c)]) void?]{
  Updates the given attribute on @racket[e] using @racket[f].
}

@defproc[(attribute-satisfies/c [attr attribute?] [f predicate/c]) contract?]{
  Produces a contract that is satisfied if the protected value
  has the attribute @racket[attr] and it satisfies @racket[f].
}

@defproc[(attribute-satisfies? [attr attribute?] [e any/c] [f predicate/c]) boolean?]{
  Returns if @racket[e] has the attribute @racket[attr] and it satisfies @racket[f].
}
