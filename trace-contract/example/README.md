# Examples

## Synthetic

These are synthetic examples intended only to illustrate trace contract functionality or subtleties.

* `increment`. A single counter that increments by 1.
* `shared`. Two counters that increment by 1 with shared state.
* `factory`. Counter factory that increments by 1 where each counter has independent state.
* `factory-shared`. Counter factory that increments by 1 where each counter shares state with all others.
* `increase`. Counter factory that increments by its factory argument with shared state.
* `string`. Single counter that increments by 1 where each string argument has a different state.
* `step`. Single counter that increments by whatever the setter sets the step size.
* `merge`. Demonstrates both tagging with `list/t` and merging with `trace-merge`.
* `fold`. Similar to `increment` except it implements the predicate with a folding function.
* `fold-global`. Folds that depend only on global traces will also have a global accumulator.
* `indy-ignore`. Shows that a collector used directly as a dependend-upon `->i` argument won't add to the trace twice.
* `suspect`. The error message should list the potential suspects alongside blame.
* `reset`. Tests the reset behavior of different clauses.
* `explain`. Uses an `#:explain` option to deliver a custom error message.
* `all`. Only trigger a clause when all the dependencies have been received.
* `combine`. Use the `combine` to get a union of clauses.
* `multi-blame`. Using multiple dependencies in an `accumulate` clause can blame multiple parties.
* `nested`. Two trace contracts where the inner depends on the outer.

## Literature

These are examples from other papers that can be implemented using trace contracts.

* `free` (*Disney et al. p.3*). A resource cannot be freed before alloc'd and cannot be double freed.
* `files` (*Disney et al. p.3*). You cannot call close, read, or write after a file has been already closed.
* `lock` (*Disney et al. p.10*). Acquire and release locks. This is a variation that supports reentrant locking.
* `has-next` (*Chen, Rosu p.15*). Requires that the client call `has-next` on an iterator before calling `next!`.
* `mutable-key` (*Chen, Rosu p.15*, *Ancona, Ferrando, Mascardi (2023)*). The hash code of an object should not be changed while it is a key in a hash.
* `take5`. (*Dimoulas et al. p.9*) Modified version of the protocol from the Take 5 game. It uses trace threading to define the predicate.
* `ensure`. (*Scholliers et al. p.6*) Combination trace and parameterize contracts used to ensure a function is called.

## Motivation

These are our own examples that demonstrate some interesting applications of trace contracts.

* `injective`. A function is injective. (Note: Surjectivity is not monitorable for infinite codomains.)
* `partial-order`. The given comparator is a partial order (reflexive, anti-symmetric, and transitive).
* `monotone`. The given function is monotone (this uses the partial order from before).
* `bank`. A functional encoding of an OO bank account with a mixin. The contract prevents the mixin from changing the account balance.
* `bank-oo`. An OO bank account with a mixin. The contract prevents the mixin from changing the account balance.

# More Examples

* Tokenization functions can only be called when the tokenizer is started.
  For example, [`get-token-range`](https://docs.racket-lang.org/framework/Color.html#%28meth._%28%28%28lib._framework%2Fmain..rkt%29._color~3atext~3c~25~3e%29._get-token-range%29%29).
