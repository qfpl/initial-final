---
title: Backpack for initial and final encodings
date: 2018-03-15
authors: dlaing
---

## Introduction

Imagine that we want to build a data type to describe some kind of expression, and that we want to write several interpreters for those expressions.

In our case we were going to start with an expression type:
```haskell
data Term =
    Lit Int
  | Add Term Term
```
and an evaluator:
```haskell
evalTerm :: Term -> Term
```

We also want to be able to add new constructors to the data type and to be able to add new interpreters, and we want to be able to add these things without having to change our existing code.

The changes we are going to make in this post are to add the extra constructor:
```haskell
data Term =
  ...
  | Mul Term Term
  ...
```
and the extra interpreter which pretty prints our terms:
```haskell
printTerm :: Term -> String
```

This is known as ["The Expression Problem"](https://en.wikipedia.org/wiki/Expression_problem).

## The solutions

### Final encoding with tagless-final

One of the common solutions to this kind of problem is the [tagless-final style](http://okmij.org/ftp/tagless-final/index.html).

In this style we avoid writing a data type entirely, and instead write a typeclass that does the interpretation directly.
The trade-off is that we don't have values of the data type which we can manipulate, but if the only thing we want to do to with these values is interpret them in some way then tagless final style is fine.

For the base case we want to be able to interpret integer literals and the addition of things, so we write:
```haskell
class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr
```

Our interpreters will be instances of this typeclass.

The evaluator is straightforward:
```haskell
instance ExpBase Int where
  lit = id
  add = (+)
```
and we could just as easily write a pretty-printer (of dubious prettiness):
```haskell
instance ExpBase String where
  lit = show
  add x y = mconcat ["(", x, " + ", y, ")"]
```

We can also add constructors to our virtual data type by writing an additional typeclass:
```haskell
class ExpMul repr where
  mul :: repr -> repr -> repr
```
and we can extend our existing interpreters by writing instances for that typeclass:
```haskell
instance ExpMul Int where
  mul = (*)

instance ExpMul String where
  mul x y = mconcat ["(", x, " * ", y, ")"]
```

If we had some preexisting code like:
```haskell
testMe x = add (lit 3) (add x (lit 2))
```
with inferred type:
```haskell
testMe :: ExpBase tm => tm -> tm
```
then we wouldn't even need to recompile it to be able to use it in:
```haskell
otherTest = mul (lit 3) (testMe (lit 5))
```
although now we have the inferred type:
```haskell
otherTest :: (ExpBase tm, ExpMul tm) => tm -> tm
```

There are two slight drawbacks to this version of the tagless final style.

If we have a lot of different interpreters we're going to have a lot of different instances of these typeclasses.
If we're not defining them in the same module as the typeclass then we need to define them in the same module as the data type, so that we don't end up with orphan instances.

We end up with something like this:
```haskell
newtype Eval = Eval { runEval :: Int }
  deriving (Eq, Ord, Show)

instance ExpBase Eval where
  lit = Eval
  add (Eval x) (Eval y) = Eval (x + y)

instance ExpMul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)
```
and if we wanted to split things up a bit more, we are out of luck.

The other slight drawback is that while we don't have to create explicit tags to mark out which types support which interpreters, we are paying the cost for the implicit tagging that we are doing.

This happens because we are passing typeclass dictionaries all over the place.
If we use `testMe` in a context where an `Int` is expected, the `ExpBase Int` instance will be used to compute the result.
This instance is -- in theory -- being passed along to `testMe` at runtime.
You may not be affected by this if your code is simple and/or if you compile with optimisations turned up.

### Final encoding with Backpack

We're now going to solve the problem again using Backpack, partly to address these problems and partly just to play around with Backpack.
This isn't my idea at all -- I learned about it during a conversation with Ed Kmett about his use of this trick in [`coda`](https://github.com/ekmett/coda).

The best source I've found for learning about Backpack is [this](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst).
You'll need GHC 8.2 and Cabal 2.0 or later to play along. We'll also be using the `cabal new-build` functionality, which I've really been enjoying.

The short description of Backpack is this: we want to be able to leave holes in modules to be filled in later.
These holes can be data types or functions, and are called "signatures".
We want to be able to write signatures for modules, and have modules that implement those signatures, and we want to be able to mix and match those puzzle pieces pretty freely.

The most common explanation I've seen is that Backpack allows you to write a signature for the types and functions you would like from a module that dealt with String-like things, and then people can write libraries in terms of those signatures, and the users of those libraries can be the ones to pick which String-like implementation should be used.

We're going to use that machinery for something else entirely.

Let us have a go at the tagless final style using Backpack.

We first set up a signature to play the role of the typeclass parameter in our tagless final version:
```haskell
-- final-bp-repr-sig
signature Repr where

data Repr
```

This will allow us to write code that depends on an indefinite type `Repr`, which we can fill in with different types later on when we glue things together.

We're making use of Cabal's support for multiple sub-libraries within the one Cabal file to group all of this together. I've gone a bit wild with it, leading to a lot of small sub-libraries, but I haven't had problems with that so far.

Our first library just packages up the `Repr` signature:
```
library final-bp-repr-sig
  signatures:          Repr
```

We now move to describing our base functionality.  Just like in the tagless final case, we want to be able to produce "things" from integer literals and from pairs of "things":
```haskell
-- final-bp-base-sig
signature Base where

import Repr

lit :: Int -> Repr
add :: Repr -> Repr -> Repr
```

We export the `Base` signature and depend on the `Repr` signature:
```
library final-bp-base-sig
  signatures:          Base
  build-depends:       final-bp-repr-sig
```

This can be used to create `Repr`-agnostic terms like so:
```haskell
-- final-bp-example-base
module Example.Base where

import Repr
import Base

term1 :: Repr
term1 = add (lit 8) (add (lit 1) (lit 2))
```

```
library final-bp-example-base
  exposed-modules:     Example.Base
  build-depends:       final-bp-base-sig
```
although we won't be able to use them until we depend on something that has an implementation for `Repr`.

We still need to be able to interpret these things.

Let us start with a simple evaluator.

We state that the representation that we are working towards is an `Int`.
```haskell
-- final-bp-eval
module Repr where

type Repr = Int
```
and we export the module:
```
library final-bp-eval
  exposed-modules:     Repr
```

There is now a module called `Repr` that matches the signature of `Repr`, so if we depend on `final-bp-eval` we'll be working with `type Repr = Int`, and anything else that was written in terms of the `Repr` signature will now have a type to work with.

We perhaps could have gotten into fancier uses of Backpack where modules got renamed and reexported, but I have been able to get away with name punning and sub-libraries so far, and getting into explanations of those mechanics might distract from the points I want to make.

Now that we have an implementation for `Repr`, we can implement something for the `Base` signature.
```haskell
-- final-bp-base-eval
module Base where

lit :: Int -> Int
lit = id

add :: Int -> Int -> Int
add = (+)
```

We depend on `final-bp-eval` to lock in which implementation of `Repr` we are working with, and we export the module `Base` as something that matches the `Base` signature:
```
library final-bp-base-eval
  exposed-modules:     Base
  build-depends:       final-bp-eval
```

To tie that all together into something we can run, we can re-export our example from earlier:
```haskell
module Example.Eval (
    module Example.Base
  ) where
  
import Example.Base
```
but in a sub-library where we depend on our `Repr`-agnostic example and our `Repr`-aware evaluator:
```
library final-bp-example-eval
  exposed-modules:     Example.Eval
  build-depends:       final-bp-base-eval final-bp-example-base
```

If we run `cabal new-repl final-bp-example-eval` we can even take this for a spin:
```
> term1
11
```

If we then want to add multiplication in the mix, we can follow the lead of tagless final and create a new signature:

```haskell
-- final-bp-mul-sig
signature Mul where

import Repr

mul :: Repr -> Repr -> Repr
```

```
library final-bp-mul-sig
  signatures:          Mul
  build-depends:       final-bp-repr-sig
```

and then implement the signature for our `Repr` for evaluation:

```haskell
-- final-bp-mul-eval
module Mul where

mul :: Int -> Int -> Int
mul = (*)
```

```
library final-bp-mul-eval
  exposed-modules:     Mul
  build-depends:       final-bp-eval
```

And we can mix that together freely with the code that we wrote before.

Now we _really_ have no tags, and we're breaking things up like we don't have a care in the world.

### An initial encoding that isn't extendable

If we really want to be able to play with values of our expression, we can write a data type:
```haskell
data Term =
    Lit !Int
  | Add !Term !Term
  | Mul !Term !Term
  deriving (Eq, Ord, Show) 
```
although we can't extend it easily.

I'm only mentioning this here because I'll be using it as a benchmark when I poke around with performance related stuff later on.
That is also why I have the strictness annotation on the various constructors.

We can create and manipulate values of this type, and we can write as many interpreters for it as we like:
```haskell
evalTerm :: Term -> Term
evalTerm tm@(Add tm1 tm2) =
  case evalTerm tm1 of
    Lit i1 ->
      case evalTerm tm2 of
        Lit i2 -> Lit (i1 + i2)
        _ -> tm
    _ -> tm
evalTerm tm@(Mul tm1 tm2) =
  case evalTerm tm1 of
    Lit i1 ->
      case evalTerm tm2 of
        Lit i2 -> Lit (i1 * i2)
        _ -> tm
    _ -> tm
evalTerm tm =
  tm
```
although we'll be paying a price for building up and tearing down the value.

We could just as easily write `prettyTerm :: Term -> String`.  The main point of difficulty is going to be adding new constructors in a way that means we don't have to rewrite our existing interpreters, so I'll be focusing on the evaluator from this point onwards.

### Initial encoding with classy `Prism`s

With the initial encoding one of the main challenges is in being extensible in the constructors that are available to our users.

We're going to be using `Prism`s (and other optics) from the `lens` package to make this happen.
I'm assuming a little bit of familiarity with `lens` here, so if you're not there yet feel free to skip ahead to the  benchmarks for the final encoding.

We'll create a data type to wrap up our constructors, which we are using to create a fixed-point of `f`:

```haskell
newtype Term f = Term { unTerm :: f (Term f) }

makeWrapped ''Term
```

The use of `makeWrapped` gives us access to an `Iso` named `_Wrapped` that allows us to wrap and unwrap this newtype.
We'll see that in use shortly.

Next we create a type for fragment of our expression that deals with literals and addition:
```haskell
data BaseF f =
    TmLit !Int
  | TmAdd !f !f
  deriving (Eq, Ord, Show)

makePrisms ''BaseF
```

The type variable `f` is going to be filled in with a `Term g` of some sort, although we don't need to worry about that too much.

The use of `makePrisms` gives us 
```haskell
_TmLit :: Prism' BaseF Int
_TmAdd :: Prism' BaseF (f, f)
```
which we can use to create values of `BaseF` and to pattern match on them.

If you haven't seen `Prism`s before, all you need to know is that we can use `review` with these prisms to build up a value of type `BaseF`:
```
> review _TmLit 2
TmLit 2
> review _TmAdd (review _TmLit 2) (review _TmLit 3)
TmAdd (TmLit 2) (TmLit 3)
```
and we can use `preview` to pattern match on those values
```
> preview _TmLit (TmLit 2)
Just 2
> preview _TmLit (TmAdd (TmLit 2) (TmLit 3))
Nothing
```

If we now wrote `type MyTerm = Term BaseF` we'd have a `Term` we could work with.

We can make some of this a bit tidier using a variant on the "classy `Prism`s" approach.

We write a typeclass such that if we supply a `Prism` from some `tm` to `BaseF`, we can produce `Prism`s for our constructors that work in the world of `Term`s.

An example might help.

We can write:
```haskell
class HasBaseF tm where
  _BaseF :: Prism' (tm f) (BaseF f)

  _Lit :: Prism' (Term tm) Int
  _Lit = _Wrapped . _BaseF . _TmLit
  {-# INLINE _Lit #-}

  _Add :: Prism' (Term tm) (Term tm, Term tm)
  _Add = _Wrapped . _BaseF . _TmAdd
  {-# INLINE _Add #-}
```
so that we only have to supply an implementation of `_BaseF` in order to make use of `_Lit` and `_Add`.

We have `INLINE` pragmas here to keep the cost of using the typeclass down.  It seems to be standard for classy `Prism`s approaches, and we'll see what that does for us when we start benchmarking this code.

The simplest instance is for the case where `tm` _is_ `BaseF`, which is what we need to work with the `MyTerm` type specified above:
```haskell
instance HasBaseF BaseF where
  _BaseF = id
  {-# INLINE _BaseF #-}
```

We'll also write some helper functions to build up terms where we have access to an instance of this typeclass:
```haskell
lit :: HasBaseF tm => Int -> Term tm
lit = review _Lit

add :: HasBaseF tm => Term tm -> Term tm -> Term tm
add tm1 tm2 = review _Add (tm1, tm2)
```

This can be used to build up value of type `Term tm` whenever we have a `HasBaseF` instance for `tm`.

```haskell
> :t add (lit 2) (lit 3)
HasBaseF tm => Term tm
> add (lit 2) (lit 3) :: MyTerm
Term (TmAdd (Term (TmLit 2)) (Term (TmLit 3)))
```

We can do the same thing for the fragment of our expression that deals with multiplication:
```haskell
data MulF f =
    TmMul !f !f
  deriving (Eq, Ord, Show)

makePrisms ''MulF

class HasMulF tm where
  _MulF :: Prism' (tm f) (MulF f)

  _Mul :: Prism' (Term tm) (Term tm, Term tm)
  _Mul = _Wrapped . _MulF . _TmMul
  {-# INLINE _Mul #-}

instance HasMulF MulF where
  _MulF = id
  {-# INLINE _MulF #-}

mul :: HasMulF tm => Term tm -> Term tm -> Term tm
mul tm1 tm2 = review _Mul (tm1, tm2)
```

If we want to use both of these fragments together, we can write a type to combine both of the intermediate types, and then write the relevant typeclass instances:
```haskell
data BMF f =
    BMBase !(BaseF f)
  | BMMul !(MulF f)
  deriving (Eq, Ord, Show)

makePrisms ''BMF

instance HasBaseF BMF where
  _BaseF = _BMBase
  {-# INLINE _BaseF #-}

instance HasMulF BMF where
  _MulF = _BMMul
  {-# INLINE _MulF #-}
```
and we can use that with the helper functions that we defined earlier
```
> lit 2 :: Term BMF
Term (BmBase (TmLit 2))
```

#### Writing the evaluator

We're going to use open recursion and a dash of laziness to write our evaluator.

We'll define a type for our evaluation rules:
```haskell
newtype EvalRule tm =
  EvalRule ((tm -> tm) -> tm -> Maybe tm)
```
which takes the evaluation function and a term and, if the rule applies, will return the evaluation of that term.

This is a rule for evaluation using big-step semantics.
Our rules fully evaluate terms, and they have access to the final evaluation function -- which we are building out of these rules -- in order to evaluate subterms when that is required.

We have a different version of this type which is in continuation-passing style:
```haskell
newtype EvalRuleK tm =
  EvalRuleK (forall r. (tm -> tm) -> (tm -> r) -> (tm -> r) -> tm -> r)
```
which will help us get a performance boost.

The constructor takes an evaluator function, a function to run if the rule applies, a function to run if the rule does not apply, and a term.  If the rule matches the first function will be called with the evaluated term, otherwise the second function will be called with the input term.  We'll see the effect of that shortly.

We have a `Monoid` instance for this type to help carry out the work of combining these rules:
```haskell
instance Monoid (EvalRuleK tm) where
  mempty =
    EvalRuleK $ \_ _ bad -> 
      bad
  mappend (EvalRuleK r1) (EvalRuleK r2) =
    EvalRuleK $ \e good bad -> 
      r1 e good (r2 e good bad)
```
and we have a function that converts our easy-to-define rules into our efficient rules:
```haskell
toEvalK :: EvalRule tm -> EvalRuleK tm
toEvalK (EvalRule f) =
  EvalRuleK $ \e good bad tm ->
    maybe (bad tm) good . f e $ tm
```

The last piece of the puzzle is a function that turn our efficient rules into an evaluator:
```haskell
mkEval :: EvalRuleK tm -> tm -> tm
mkEval (EvalRuleK f) =
  let
    step = f eval Just (const Nothing)
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
```

This makes use of laziness to make the evaluation function available to each of the rules, even though the rules 
are used to define the evaluation function.

The stage is now set, and we can now write the evaluation rules for the various pieces of our AST.

The rules for `BaseF` handle addition and make use of the `Prism`s from the `HasBaseF` typeclass to deconstruct the term:
```haskell
-- Base.Eval
addRule :: HasBaseF tm => EvalRule (Term tm)
addRule = EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Add tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 + i2)

evalRules :: HasBase f => EvalRuleK (Term f)
evalRules = toEvallK addRule
```
and the rules for `MulF` are similar:
```haskell
-- Mul.Eval
mulRule :: (HasBaseF tm, HasMulF tm) => EvalRule (Term tm)
mulRule = EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Mul tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 * i2)

evalRules :: HasBase f => EvalRuleK (Term f)
evalRules = toEvallK mulRule
```

We can combine these rules if we are using both of those pieces at the same time:
```haskell
-- Combined.Eval
import qualified Base.Eval as B
import qualified Mul.Eval  as M

evalRules :: (HasBaseF tm, HasMulF tm) => EvalRuleK (Term f)
evalRules =
  mappend B.evalRules M.evalRules
```
and if we had more fragments that we wanted to combine we would add them in here as well.

### Initial encoding with classy `Prism`s - another approach

There is an alternate approach which is worth mentioning.

If we are happy to lose the convenience of intermediate data types likes `BaseF`, we can have typeclasses that expose `Prism`s to the constructors that we are interested in directly.
This will mean we have fewer structures in our data to walk over, which can be handy for performance.

In our case this leads to the classes:
```haskell
class HasBase tm where
  _Lit :: Prism' (Term tm) Int
  _Add :: Prism' (Term tm) (Term tm, Term tm)
```
and
```haskell
class HasMul tm where
  _Mul :: Prism' (Term tm) (Term tm, Term tm)
```

We have to roll our own data type to use these:
```haskell
data TermF f =
    BMLit !Int
  | BMAdd !f !f
  | BMMul !f !f
  deriving (Eq, Ord, Show)

makePrisms ''TermF
```
and wire up the appropriate instances:
```haskell
instance HasBase TermF where
  _Lit = _Wrapped . _BMLit
  {-# INLINE _Lit #-}
  _Add = _Wrapped . _BMAdd
  {-# INLINE _Add #-}

instance HasMul TermF where
  _Mul = _Wrapped . _BMMul
  {-# INLINE _Mul #-}
```
but otherwise nothing much else changes.

### Initial encoding with "Backpacky `Prism`s"

Let us translate that into code that uses Backpack.

We'll start with a signature that gives a name to our `Term` type
```haskell
-- initial-bp-term-sig
signature Term.Type where

data Term
```

```
library initial-bp-term-sig
  signatures: Term.Type
```

We'll build on that to create a signature that exposes the `Prism`s that we want for the base fragment of our expression:
```haskell
-- initial-bp-base-sig
signature Base.Type where

import Term.Type

import Control.Lens

_Lit :: Prism' Term Int
_Add :: Prism' Term (Term, Term)
```
which will require an implementation for the `Term` signature before we can use it.
```
library initial-bp-base-sig
  signatures:    Base.Type
  build-depends: initial-bp-term-sig, lens
```

We can build on this with the usual helper functions:
```haskell
-- initial-bp-base
module Base where

import Term.Type
import Base.Type

import Control.Lens

lit :: Int -> Term
lit = review _Lit

add :: Term -> Term -> Term
add x y = review _Add (x, y)
```

```
library initial-bp-base
  exposed-modules: Base
  build-depends:   initial-bp-base-sig, lens
```

If we want to start implementing these things, we can create an implementation for `Term.Type`:
```haskell
-- initial-bp-example-term-base
module Term.Type where

import Control.Lens

data Term =
    Lit !Int
  | Add !Term !Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
```
and then reexport the `Prism`s to create an implementation for `Base.Type`:
```haskell
-- initial-bp-example-term-base
module Base.Type (
    _Lit
  , _Add
  ) where

import Term.Type
```

```
library initial-bp-example-term-base
  exposed-modules: Term.Type, Base.Type
  build-depends:   lens
```

We can build up the same machinery for the fragment which deals with multiplication:
```haskell
-- initial-bp-mul-sig
signature Mul.Type where

import Term.Type

import Control.Lens

_Mul :: Prism' Term (Term, Term)
```

```
library initial-bp-mul-sig
  signatures:    Mul.Type
  build-depends: initial-bp-term-sig, lens
```
including the usual helper function:
```haskell
-- initial-bp-mul
module Mul where

import Term.Type
import Mul.Type

import Control.Lens

mul :: Term -> Term -> Term
mul x y = review _Mul (x, y)
```

```
library initial-bp-mul
  exposed-modules: Mul
  build-depends:   initial-bp-mul-sig, lens
```

Then we can create a new implementation for `Term.Type`:
```haskell
-- initial-bp-example-term-base-mul
module Term.Type where

import Control.Lens

data Term =
    Lit !Int
  | Add !Term !Term
  | Mul !Term !Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
```
and reexport the `Prism`s for the base fragment:
```haskell
-- initial-bp-example-term-base-mul
module Base.Type (
    _Lit
  , _Add
  ) where

import Term.Type
```
and for the multiplication fragment:
```haskell
-- initial-bp-example-term-base-mul
module Mul.Type (
  , _Mul
  ) where

import Term.Type
```

```
library initial-bp-example-term-base-mul
  exposed-modules: Term.Type, Base.Type, Mul.Type
  build-depends:   lens
```

We can capture our rules types and associated functions in a sub-library (`initial-bp-eval`), and then we can write the rules for our fragments as we did before:
```haskell
-- initial-bp-base-eval
module Base.Eval where

import Control.Lens

import Interpret.Eval

import Term.Type
import Base.Type

addRule :: EvalRule Term
addRule = EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Add tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 + i2)

evalRules :: EvalRuleK Term
evalRules = toEvalK addRule
```
where we are generic in the `Term` and `Base` signatures:
```
library initial-bp-base-eval
  exposed-modules: Base.Eval
  build-depends:   initial-bp-term-sig, initial-bp-base-sig, initial-bp-eval, lens
```

## Benchmarking the code

We have a few solutions to the same problem, so the natural question is: how do they differ?
Aside from the various trade-offs that have been mentioned already, the most exciting of these differences will come from the relative performance.

The code was benchmarked using `criterion`.
There were a number of benchmarks that were used, but I'll be focusing on this one:
```haskell
evalAddMulBig tm =
  evalTerm $ 
    add (mul (add tm (lit 3)) 
             (add tm (lit 5))) 
        (mul (add tm (lit 7)) 
             (add tm (lit 11)))
```
where the benchmark itself looks something like:
```haskell
  nf evalAddMulBig (lit 2)
```

### Final encoding

#### Benchmark results

The tagless final approach on `Int`
```
time                 3.463 ns   (3.439 ns .. 3.488 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.472 ns   (3.450 ns .. 3.505 ns)
std dev              91.75 ps   (68.67 ps .. 118.7 ps)
variance introduced by outliers: 46% (moderately inflated)
```
and on a newtype wrapping `Int`
```
time                 3.222 ns   (3.201 ns .. 3.245 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.215 ns   (3.200 ns .. 3.235 ns)
std dev              59.28 ps   (50.17 ps .. 69.78 ps)
variance introduced by outliers: 29% (moderately inflated)
```
and the version using Backpack
```
time                 3.452 ns   (3.433 ns .. 3.472 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.447 ns   (3.429 ns .. 3.476 ns)
std dev              80.32 ps   (64.39 ps .. 105.0 ps)
variance introduced by outliers: 39% (moderately inflated)
```
were all very similar.

#### Looking at the generated Core

If we have a look at the Core which was produced, we can see why they are so similar (and so very fast).

This is the version for the final encoding on an Int:
```
evalAddMulBig :: Int -> Int
evalAddMulBig
  = \ (tm :: Int) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }
```
which is identical to the version using Backpack (since `Repr` is `Int` in this case):
```
evalAddMulBig :: Repr -> Repr
evalAddMulBig
  = \ (tm :: Repr) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }
```
and is the same as for the final encoding on a newtype around an Int with the exception of a few casts:
```
evalAddMulBig1 :: Eval -> Int
evalAddMulBig1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }

evalAddMulBig :: Eval -> Eval
evalAddMulBig = evalAddMulBig1 `cast` <Co:18>
```

What we're looking at here is code that breaks open the usual `Int` type to get hold of the unpacked machine `Int` (`I#`), and everything else from that point on is happening in terms of those unpacked machine `Int`s.

### Initial encoding

#### Benchmark results

We wrote the vanilla initial encoding:
```haskell
data Term =
    Lit !Int
  | Add !Term !Term
  | Mul !Term !Term
  deriving (Eq, Ord, Show) 
```
and its `evalTerm` function so that we had a non-extensible solution to make comparisons against.

The vanilla initial encoding was slower that the final encodings
```
time                 58.13 ns   (57.77 ns .. 58.51 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 58.33 ns   (57.95 ns .. 58.86 ns)
std dev              1.445 ns   (1.066 ns .. 1.822 ns)
variance introduced by outliers: 38% (moderately inflated)
```
which makes sense, since it needed to build up and tear down values for the expressions.

The versions using classy `Prism`s
```
time                 63.02 ns   (61.71 ns .. 64.53 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 63.38 ns   (62.37 ns .. 64.48 ns)
std dev              3.693 ns   (2.988 ns .. 4.670 ns)
variance introduced by outliers: 77% (severely inflated)
```
was a touch slower than the vanilla version, and the version using Backpack
```
time                 55.35 ns   (54.95 ns .. 55.79 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 55.59 ns   (55.17 ns .. 56.07 ns)
std dev              1.567 ns   (1.184 ns .. 1.932 ns)
variance introduced by outliers: 44% (moderately inflated)
```
was faster!

The version using classy `Prism`s via the convenience types for the various pieces:
```haskell
data TermF f =
    BMBase !(BaseF f)
  | BMMul  !(MulF f)
```
fared a bit worse:
```
time                 80.63 ns   (80.03 ns .. 81.22 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 80.61 ns   (80.09 ns .. 81.36 ns)
std dev              2.083 ns   (1.624 ns .. 2.820 ns)
variance introduced by outliers: 39% (moderately inflated)
```
which we assumed was due to the extra data structures that had to be built up and traversed.

#### Looking at the generated Core

Again, looking at the core will be useful for understanding the differences between the benchmark results.

We'll start looking at our evaluation function, which was called `evalTerm` in our code.

The version generated for the vanilla initial encoding:
```
Rec {
evalTerm :: Term -> Term
evalTerm
  = \ (tm :: Term) ->
      case tm of wild {
        Lit ipv -> wild;
        Add tm1 tm2 ->
          case evalTerm tm1 of {
            __DEFAULT -> wild;
            Lit dt ->
              case evalTerm tm2 of {
                __DEFAULT -> wild;
                Lit dt1 -> Lit (+# dt dt1)
              }
          };
        Mul tm1 tm2 ->
          case evalTerm tm1 of {
            __DEFAULT -> wild;
            Lit dt ->
              case evalTerm tm2 of {
                __DEFAULT -> wild;
                Lit dt1 -> Lit (*# dt dt1)
              }
          }
      }
end Rec }
```
and for the version using Backpack: 
```
Rec {
evalTerm :: Term -> Term
evalTerm
  = \ (tm1 :: Term) ->
      case tm1 of wild {
        Lit ipv -> wild;
        Add y1 y2 ->
          case evalTerm y1 of {
            __DEFAULT -> wild;
            Lit dt ->
              case evalTerm y2 of {
                __DEFAULT -> wild;
                Lit dt1 -> Lit (+# dt dt1)
              }
          };
        Mul y1 y2 ->
          case evalTerm y1 of {
            __DEFAULT -> wild;
            Lit dt ->
              case evalTerm y2 of {
                __DEFAULT -> wild;
                Lit dt1 -> Lit (*# dt dt1)
              }
          }
      }
end Rec }
```
are identical, which I was not expecting.

The version using classy `Prism`s and no intermediate data structures is more or less the same except for the casts:
```
Rec {
evalTerm :: Term TermF -> Term TermF
evalTerm
  = \ (tm1 :: Term TermF) ->
      case tm1 `cast` <Co:2> of wild {
        BMLit ipv -> wild `cast` <Co:3>;
        BMAdd y1 y2 ->
          case (evalTerm y1) `cast` <Co:2> of {
            __DEFAULT -> wild `cast` <Co:3>;
            BMLit dt ->
              case (evalTerm y2) `cast` <Co:2> of {
                __DEFAULT -> wild `cast` <Co:3>;
                BMLit dt1 -> (BMLit (+# dt dt1)) `cast` <Co:3>
              }
          };
        BMMul y1 y2 ->
          case (evalTerm y1) `cast` <Co:2> of {
            __DEFAULT -> wild `cast` <Co:3>;
            BMLit dt ->
              case (evalTerm y2) `cast` <Co:2> of {
                __DEFAULT -> wild `cast` <Co:3>;
                BMLit dt1 -> (BMLit (*# dt dt1)) `cast` <Co:3>
              }
          }
      }
end Rec }
```

The version using the intermediate data structures has to do the work of walking through those data structures:
```
Rec {
evalTerm :: Term TermF -> Term TermF
evalTerm
  = \ (tm1 :: Term TermF) ->
      case tm1 `cast` <Co:2> of wild {
        BMBase y1 ->
          case y1 of {
            TmLit dt -> wild `cast` <Co:3>;
            TmAdd y2 y3 ->
              case (evalTerm y2) `cast` <Co:2> of {
                BMBase y4 ->
                  case y4 of {
                    TmLit dt ->
                      case (evalTerm y3) `cast` <Co:2> of {
                        BMBase y5 ->
                          case y5 of {
                            TmLit dt1 -> (BMBase (TmLit (+# dt dt1))) `cast` <Co:3>;
                            TmAdd ipv ipv1 -> wild `cast` <Co:3>
                          };
                        BMMul ipv -> wild `cast` <Co:3>
                      };
                    TmAdd ipv ipv1 -> wild `cast` <Co:3>
                  };
                BMMul ipv -> wild `cast` <Co:3>
              }
          };
        BMMul ipv ->
          case ipv of { TmMul x1 x2 ->
          case (evalTerm x1) `cast` <Co:2> of {
            BMBase y1 ->
              case y1 of {
                TmLit dt ->
                  case (evalTerm x2) `cast` <Co:2> of {
                    BMBase y2 ->
                      case y2 of {
                        TmLit dt1 -> (BMBase (TmLit (*# dt dt1))) `cast` <Co:3>;
                        TmAdd ipv1 ipv2 -> wild `cast` <Co:3>
                      };
                    BMMul ipv1 -> wild `cast` <Co:3>
                  };
                TmAdd ipv1 ipv2 -> wild `cast` <Co:3>
              };
            BMMul ipv1 -> wild `cast` <Co:3>
          }
          }
      }
end Rec }
```
which helps explain why it is slower than the other approaches.

In order to dig further into the differences in the benchmarks, we have to look at how `evalTerm` is used.

The Core for the vanilla initial encoding uses `evalTerm` pretty directly:
```
evalAddBig2 :: Term
evalAddBig2 = Lit 3#

evalAddBig1 :: Term
evalAddBig1 = Lit 5#

evalAddMulBig2 :: Term
evalAddMulBig2 = Lit 7#

evalAddMulBig1 :: Term
evalAddMulBig1 = Lit 11#

evalAddMulBig :: Term -> Term
evalAddMulBig
  = \ (tm :: Term) ->
      case tm of dt { __DEFAULT ->
      evalTerm
        (Add
           (Mul (Add dt evalAddBig2) (Add dt evalAddBig1))
           (Mul (Add dt evalAddMulBig2) (Add dt evalAddMulBig1)))
      }
```

The Core for the Backpack solution is interesting. It is mostly the same as the vanilla initial encoding.

Instead of `evalTerm (Add x y)` we have `evalAddBig_$evalTerm x y`, where `evalAddBig_$evalTerm` is a partial unfolding of the rule for dealing with addition.  This explains why this version is slightly faster than the vanilla initial encoding.

```
evalAddBig2 :: Term
evalAddBig2 = Lit 3#

evalAddBig1 :: Term
evalAddBig1 = Lit 5#

evalAddMulBig2 :: Term
evalAddMulBig2 = Lit 7#

evalAddMulBig1 :: Term
evalAddMulBig1 = Lit 11#

evalAddBig_$sevalTerm :: Term -> Term -> Term
evalAddBig_$sevalTerm
  = \ (sc :: Term) (sc1 :: Term) ->
      case evalTerm sc of {
        __DEFAULT -> Add sc sc1;
        Lit dt ->
          case evalTerm sc1 of {
            __DEFAULT -> Add sc sc1;
            Lit dt1 -> Lit (+# dt dt1)
          }
      }

evalAddMulBig :: Term -> Term
evalAddMulBig
  = \ (tm :: Term) ->
      case tm of dt { __DEFAULT ->
      evalAddBig_$sevalTerm
        (Mul (Add dt evalAddBig2) (Add dt evalAddBig1))
        (Mul (Add dt evalAddMulBig2) (Add dt evalAddMulBig1))
      }
```

The same is true for the version using classy `Prism`s, although it has a number of casts mixed through it:
```
evalAddMulBig4 :: TermF (Term TermF)
evalAddMulBig4 = BMLit 3#

evalAddMulBig3 :: TermF (Term TermF)
evalAddMulBig3 = BMLit 5#

evalAddMulBig2 :: TermF (Term TermF)
evalAddMulBig2 = BMLit 7#

evalAddMulBig1 :: TermF (Term TermF)
evalAddMulBig1 = BMLit 11#

evalAddBig_$sevalTerm
  :: Term TermF
     -> Term TermF
     -> (TermF (Term TermF) :: *) ~R# (Term TermF :: *) => Term TermF
evalAddBig_$sevalTerm
  = \ (sc :: Term TermF)
      (sc1 :: Term TermF)
      (sg :: (TermF (Term TermF) :: *) ~R# (Term TermF :: *)) ->
      case (evalTerm sc) `cast` <Co:2> of {
        __DEFAULT -> (BMAdd sc sc1) `cast` <Co:3>;
        BMLit dt ->
          case (evalTerm sc1) `cast` <Co:2> of {
            __DEFAULT -> (BMAdd sc sc1) `cast` <Co:3>;
            BMLit dt1 -> (BMLit (+# dt dt1)) `cast` <Co:3>
          }
      }

evalAddMulBig :: Term TermF -> Term TermF
evalAddMulBig
  = \ (tm :: Term TermF) ->
      case tm `cast` <Co:2> of nt { __DEFAULT ->
      evalAddBig_$sevalTerm
        ((BMMul
            ((BMAdd (nt `cast` <Co:3>) (evalAddMulBig4 `cast` <Co:489>))
             `cast` <Co:541>)
            ((BMAdd (nt `cast` <Co:3>) (evalAddMulBig3 `cast` <Co:489>))
             `cast` <Co:541>))
         `cast` <Co:541>)
        ((BMMul
            ((BMAdd (nt `cast` <Co:3>) (evalAddMulBig2 `cast` <Co:489>))
             `cast` <Co:541>)
            ((BMAdd (nt `cast` <Co:3>) (evalAddMulBig1 `cast` <Co:489>))
             `cast` <Co:541>))
         `cast` <Co:541>)
        @~ <Co:541>
      }
```

The version using the intermediate data structures had to do more work, both when creating and when walking through those data structures:
```
evalAddBig4 :: BaseF (Term TermF)
evalAddBig4 = TmLit 3#

evalAddBig2 :: BaseF (Term TermF)
evalAddBig2 = TmLit 5#

evalAddBig3 :: TermF (Term TermF)
evalAddBig3 = BMBase evalAddBig4

evalAddBig1 :: TermF (Term TermF)
evalAddBig1 = BMBase evalAddBig2

evalAddMulBig4 :: BaseF (Term TermF)
evalAddMulBig4 = TmLit 7#

evalAddMulBig2 :: BaseF (Term TermF)
evalAddMulBig2 = TmLit 11#

evalAddMulBig3 :: TermF (Term TermF)
evalAddMulBig3 = BMBase evalAddMulBig4

evalAddMulBig1 :: TermF (Term TermF)
evalAddMulBig1 = BMBase evalAddMulBig2

evalAddBig_$sevalTerm
  :: Term TermF
     -> Term TermF
     -> (TermF (Term TermF) :: *) ~R# (Term TermF :: *) => Term TermF
evalAddBig_$sevalTerm
  = \ (sc :: Term TermF)
      (sc1 :: Term TermF)
      (sg :: (TermF (Term TermF) :: *) ~R# (Term TermF :: *)) ->
      case (evalTerm sc) `cast` <Co:2> of {
        BMBase y1 ->
          case y1 of {
            TmLit dt ->
              case (evalTerm sc1) `cast` <Co:2> of {
                BMBase y2 ->
                  case y2 of {
                    TmLit dt1 -> (BMBase (TmLit (+# dt dt1))) `cast` <Co:3>;
                    TmAdd ipv ipv1 -> (BMBase (TmAdd sc sc1)) `cast` <Co:3>
                  };
                BMMul ipv -> (BMBase (TmAdd sc sc1)) `cast` <Co:3>
              };
            TmAdd ipv ipv1 -> (BMBase (TmAdd sc sc1)) `cast` <Co:3>
          };
        BMMul ipv -> (BMBase (TmAdd sc sc1)) `cast` <Co:3>
      }

evalAddMulBig :: Term TermF -> Term TermF
evalAddMulBig
  = \ (tm :: Term TermF) ->
      case tm `cast` <Co:2> of nt { __DEFAULT ->
      evalAddBig_$sevalTerm
        ((BMMul
            (TmMul
               ((BMBase (TmAdd (nt `cast` <Co:3>) (evalAddBig3 `cast` <Co:499>)))
                `cast` <Co:531>)
               ((BMBase (TmAdd (nt `cast` <Co:3>) (evalAddBig1 `cast` <Co:499>)))
                `cast` <Co:531>)))
         `cast` <Co:531>)
        ((BMMul
            (TmMul
               ((BMBase
                   (TmAdd (nt `cast` <Co:3>) (evalAddMulBig3 `cast` <Co:499>)))
                `cast` <Co:531>)
               ((BMBase
                   (TmAdd (nt `cast` <Co:3>) (evalAddMulBig1 `cast` <Co:499>)))
                `cast` <Co:531>)))
         `cast` <Co:531>)
        @~ <Co:531>
      }
```

## Conclusion

I learned a few things from this.

Final encodings can be blazingly fast.
I think I'd heard this before, but running some benchmarks and dumping the generated Core really hammered that home for me.
Backpack lets us decompose final encodings a little more than we can with the usual approach, although we are partly trading off newtype wrapping for sub-libraries.
I still really like the usage of it.

Classy `Prism`s (or the Backpack equivalent) let us write extensible code with performance that is competitive -- or better than -- the equivalent non-extensible code.  I wasn't expecting that at all.

I have a few projects where I use classy `Prism`s and open-recursion rules systems to build up little languages from these pieces, so I'm keen to roll out the continuation-based rules systems in the next version of that project and see how they perform.  The rules system could present a simpler API to the people using them, so that's something I'll work on at the same time.

Mostly, I wrote this because I liked the look of the Backpack version of the tagless final style that Ed showed me,
and I wanted to see if I could do similar cool things with Backpack.

If you find yourself wanting to poke around with the code mentioned in this post, the repository is [here](https://github.com/qfpl/initial-final).

It seems like there is much more to explore.

We could try to build something like [Trees that grow (PDF)](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf), with this being a rough sketch of how it might go:
```
signature AddPiece where

import Control.Lens

import Term.Type

data AddAnnotation

_Add'   :: Prism' Term (AddAnnotation, (Term, Term))

_AddAnn :: Prism' Term AddAnnotation
_AddAnn = _Add' . _1

_Add    :: Prism' Term (Term, Term)
_Add    = _Add' . _2
```
We could fill in the `Prism`s and leave the `AddAnnotation` to be filled in later, and there is probably all sorts of other fun to be had in that space.

In other areas, Ed has also done something interesting [here](https://github.com/ekmett/coda/blob/master/lib/coda-set/Elem.hsig) and [here](https://github.com/ekmett/coda/blob/master/lib/coda-set/Set/Internal.hs#L248).

This is a copy of the code for `Set` from `containers`, where the element type is fixed via a Backpack signature.
The twist is that this will `{-# UNPACK #-}` the element type into the definition of the `Set`.
I haven't played with it yet, but it seems like a really interesting idea. 

It feels like there is a lot of applications and techniques involving Backpack that are yet to be discovered. Hopefully this gives some folks the motivation to go digging.
