% The expression problem with Backpack

### The problem

### The solutions

#### Final encoding with tagless-final

One of the common solutions to this kind of problem is the [tagless-final style](http://okmij.org/ftp/tagless-final/index.html).

In this style we avoid writing a data type entirely, and write a typeclass that focuses on interpreting the data type.
If we only needed to manipulate the values of our expression type to interpret values of that type, then this lets us skip having to define the type and create values of that types entirely.

For the base case we want to be able to produce "things" from integer literals and from pairs of "things", so we write:
```haskell
class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr
```

We write interpreters as instances of this typeclass.

The evaluator is straighforward:
```haskell
instance ExpBase Int where
  lit = id
  add = (+)
```
but we could just as easily write a pretty-printer (of dubious prettiness):
```haskell
instance ExpBase String where
  lit = show
  add x y = mconcat ["(", x, " + ", y, ")"]
```

We can also add to our virtual data type by writing addition typeclass:
```haskell
class ExpMul repr where
  mul :: repr -> repr -> repr
```
and then, if we want, we can extend our exisiting interpreters:
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
then we wouldn't even need to recompile it to be able to use it in
```haskell
mul (lit 3) (testMe (lit 5))
```

A slight drawback to that approach arises from the orphan instance problem.
If we have a lot of different interpreters, and we're not writing them alongside the definitions of the interpreters, we're going to have a lot of different types that have instances of these typeclasses.

If we're not defining them in the same module as the typeclass for our interpreter then we need to define them in the same module as the data type, so that we don't end up with orphan instances.

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
where we may have wanted to split things up a bit more.

The other slight drawback is that while we don't have to create explicit tags to mark out which types support which interpreters, we are paying the cost for the implicit tagging that we are doing.
This happens because we are passing typeclass dictionaries all over the place.

#### Final encoding with Backpack

We're going to use Backpack to get around that problem.
This isn't my idea at all -- I learned about it during a conversation with Ed Kmett about his use of this trick in `coda`.

It's going to make some very simple uses of Backpack, but it's pretty neat.  The best source I've found for reasoning about how to make use of Backpack is [this](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst), in case you wanted to follow along. You'll need GHC 8.2 and Cabal 2.0 or greater to play along.

The short description of Backpack is this:  we want to be able to write signatures for modules, and have modules that implement those signatures, and we want to be able to mix and match those puzzle pieces pretty freely.
If I want a signature to depend on two other signatures, and then I provide an implementation for one of those signatures then I should be able to.

Backpack allows you to write a signature for the types and functions you would like from a module that dealt with String-like things, and then people can write libraries in terms of those signatures, and the users of those libraries can be the ones to pick which String-like implementation should be used.

We first set up a signature to play the role of the typeclass variable in our tagless final version:
```haskell
-- repr
signature Repr where

data Repr
```
This will allow us to write code that depends on an indefinite type `Repr`, which we can fill in with different types later on when we glue things together.

We're making use of Cabal's support for multiple sub-libraries within the one Cabal file to group all of this together. I've gone a bit wild with it, leading to a lot of small sub-libraries, but I've found I don't mind that too much.

Our first library just packages up the `Repr` signature:
```
library repr
  signatures:          Repr
```

We now move to describing our base functionality.  Just like in the tagless final case, we want to be able to produce "things" from integer literals and from pairs of "things":
```haskell
-- final-bp-base
signature Base where

import Repr

lit :: Int -> Repr
add :: Repr -> Repr -> Repr
```

We export the `Base` signature and depend on the `Repr` signature:
```
library final-bp-base
  signatures:          Base
  build-depends:       repr
```

This can be used to create `Repr`-agnostic terms like so:
```haskell
-- final-bp-example-base
haskell
module Example.Base where

import Repr
import Base

term1 :: Repr
term1 = add (lit 8) (add (lit 1) (lit 2))
```

```
library final-bp-example-base
  exposed-modules:     Example.Base
  build-depends:       final-bp-base
```

That's all well and good, but we need to be able to interpret these things.

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

We perhaps could have gotten into fancier uses of Backpack where modules got renamed and reexported, but I have been able to get away with name punning and sub-libraries so far, and getting into explanations of those mechanics might distract from the points I want to make.

There is now a module called `Repr` that matches the signature of `Repr`, so if we depend on `final-bp-eval` we'll be working with `type Repr = Int`.

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
Example.Eval > term1
11
```

If we then want to add multiplication in the mix, we can follow the lead of tagless final and create a new signature:

```haskell
-- final-bp-mul
signature Mul where

import Repr

mul :: Repr -> Repr -> Repr
```

```
library final-bp-mul
  signatures:          Mul
  build-depends:       repr
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

Now we _really_ have no tags, and we're breaking things up like we don't have a care in the world.

#### An initial encoding that isn't extendable

If we really want to be able to play with values of our term, we can write a data type for our terms:
```haskell
data Term =
    Lit Int
  | Add Term Term
  | Mul Term Term
  deriving (Eq, Ord, Show) 
```
although we can't extend it easily.

We can create and manipulate values of this type, and we can write as many interpreters for it as we like:
```haskell
evalTerm :: Term -> Term
evalTerm tm@(Add tm1 tm2) =
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 + i2)
    _ -> tm
evalTerm tm@(Mul tm1 tm2) =
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 * i2)
    _ -> tm
evalTerm tm =
  tm
```
although we'll be paying a price for building up and tearing down the value.

I'm only mentioning this here because I'll be using it as a benchmark when I poke around with performance related stuff later on.

#### Initial encoding with classy `Prism`s

```haskell
newtype Term f a = Term { unTerm :: f (Term f) a }

makeWrapped ''Term
```

```haskell
data BaseF f a =
    TmLit Int
  | TmAdd (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''BaseF
```

```haskell
class HasBaseF tm where
  _BaseF :: Prism' (tm f a) (BaseF f a)

  _Lit :: Prism' (Term tm a) Int
  _Lit = _Wrapped . _BaseF . _TmLit

  _Add :: Prism' (Term tm a) (Term tm a, Term tm a)
  _Add = _Wrapped . _BaseF . _TmAdd

instance HasBaseF BaseF where
  _BaseF = id
```

```haskell
lit :: HasBaseF tm => Int -> Term tm a
lit = review _Lit

add :: HasBaseF tm => Term tm a -> Term tm a -> Term tm a
add tm1 tm2 = review _Add (tm1, tm2)
```

```haskell
data MulF f a =
    TmMul (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''MulF
```

```haskell
class HasMulF tm where
  _MulF :: Prism' (tm f a) (MulF f  a)

  _Mul :: Prism' (Term tm a) (Term tm a, Term tm a)
  _Mul = _Wrapped . _MulF . _TmMul

instance HasMulF MulF where
  _MulF = id
```

```haskell
mul :: HasMulF tm => Term tm a -> Term tm a -> Term tm a
mul tm1 tm2 = review _Mul (tm1, tm2)
```

#### Initial encoding with Backpack

```haskell
-- initial-bp-term-sig
signature Term.Type where

data Term
```

```haskell
-- initial-bp-base-sig
signature Base.Type where

import Term.Type

import Control.Lens

_Lit :: Prism' Term Int
_Add :: Prism' Term (Term, Term)
```

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

```haskell
-- initial-bp-example-term-base
module Term.Type where

import Control.Lens

data Term =
    Lit Int
  | Add Term Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
```

```haskell
-- initial-bp-example-term-base
module Base.Type (
    _Lit
  , _Add
  ) where

import Term.Type
```

```haskell
-- initial-bp-mul-sig
signature Mul.Type where

import Term.Type

import Control.Lens

_Mul :: Prism' Term (Term, Term)
```

```haskell
-- initial-bp-mul
module Mul where

import Term.Type
import Mul.Type

import Control.Lens

mul :: Term -> Term -> Term
mul x y = review _Mul (x, y)
```

```haskell
-- initial-bp-example-term-base-mul
module Term.Type where

import Control.Lens

data Term =
    Lit Int
  | Add Term Term
  | Mul Term Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
```

```haskell
-- initial-bp-example-term-base-mul
module Base.Type (
    _Lit
  , _Add
  ) where

import Term.Type
```

```haskell
-- initial-bp-example-term-base-mul
module Mul.Type (
  , _Mul
  ) where

import Term.Type
```

### Benchmarking the code

### Looking at the core

#### Vanilla

lit1 :: Int
lit1 = I# 2#

lit2 :: Term
lit2 = Lit lit1

evalAddSmall :: Term -> Term
evalAddSmall = \ (tm :: Term) -> evalTerm (Add tm tm)

evalAddBig4 :: Int
evalAddBig4 = I# 3#

evalAddBig3 :: Term
evalAddBig3 = Lit evalAddBig4

evalAddBig2 :: Int
evalAddBig2 = I# 5#

evalAddBig1 :: Term
evalAddBig1 = Lit evalAddBig2

evalAddMulSmall :: Term -> Term
evalAddMulSmall
  = \ (tm :: Term) -> evalTerm (Add (Mul tm evalAddBig3) evalAddBig1)

evalAddBig :: Term -> Term
evalAddBig
  = \ (tm :: Term) ->
      evalTerm (Add (Add tm evalAddBig3) (Add tm evalAddBig1))

evalAddMulBig4 :: Int
evalAddMulBig4 = I# 7#

evalAddMulBig3 :: Term
evalAddMulBig3 = Lit evalAddMulBig4

evalAddMulBig2 :: Int
evalAddMulBig2 = I# 11#

evalAddMulBig1 :: Term
evalAddMulBig1 = Lit evalAddMulBig2

evalAddMulBig :: Term -> Term
evalAddMulBig
  = \ (tm :: Term) ->
      evalTerm
        (Add
           (Mul (Add tm evalAddBig3) (Add tm evalAddBig1))
           (Mul (Add tm evalAddMulBig3) (Add tm evalAddMulBig1)))
#### Final

lit1 :: Int
lit1 = I# 2#

lit2 :: Eval
lit2 = lit1 `cast` <Co:2>

evalAddSmall1 :: Eval -> Int
evalAddSmall1
  = \ (tm :: Eval) ->
      $fNumInt_$c+ (tm `cast` <Co:12>) (tm `cast` <Co:13>)

evalAddSmall :: Eval -> Eval
evalAddSmall = evalAddSmall1 `cast` <Co:18>

evalAddMulSmall1 :: Eval -> Int
evalAddMulSmall1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x -> I# (+# (*# x 3#) 5#) }

evalAddMulSmall :: Eval -> Eval
evalAddMulSmall = evalAddMulSmall1 `cast` <Co:18>

evalAddBig1 :: Eval -> Int
evalAddBig1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x -> I# (+# (+# x 3#) (+# x 5#)) }

evalAddBig :: Eval -> Eval
evalAddBig = evalAddBig1 `cast` <Co:18>

evalAddMulBig1 :: Eval -> Int
evalAddMulBig1
  = \ (tm :: Eval) ->
      case tm `cast` <Co:12> of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }

evalAddMulBig :: Eval -> Eval
evalAddMulBig = evalAddMulBig1 `cast` <Co:18>

#### Final with Backpack

lit2 :: Repr
lit2 = I# 2#

evalAddSmall :: Repr -> Repr
evalAddSmall = \ (tm :: Repr) -> $fNumInt_$c+ tm tm

evalAddMulSmall :: Repr -> Repr
evalAddMulSmall
  = \ (tm :: Repr) -> case tm of { I# x -> I# (+# (*# x 3#) 5#) }

evalAddBig :: Repr -> Repr
evalAddBig
  = \ (tm :: Repr) ->
      case tm of { I# x -> I# (+# (+# x 3#) (+# x 5#)) }

evalAddMulBig :: Repr -> Repr
evalAddMulBig
  = \ (tm :: Repr) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }

#### Initial

#### Initial with Backpack
