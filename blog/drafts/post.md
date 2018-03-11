% The expression problem with Backpack

# The problem

Imagine that we want to build a data type to describe some kind of expression, and we want to write several interpreters for those expressions.

We also want to be able to add new constructors to the data type and to be able to add new interpreters, and we want to be able to do these things while making no changes to our existing code.

In our case we were going to start with an expression type:
```haskell
data Term =
    Lit
  | Add Term Term
```
and an evaluator;
```haskell
evalTerm :: Term -> Term
```

We are then going to change the requirements so that we need an extra constructor:
```haskell
data Term =
  ...
  | Mul Term Term
  ...
```
and so that we are able to pretty print terms:
```haskell
printTerm :: Term -> String
```

# The solutions

## Final encoding with tagless-final

One of the common solutions to this kind of problem is the [tagless-final style](http://okmij.org/ftp/tagless-final/index.html).

In this style we avoid writing a data type entirely, and write a typeclass that focuses on interpreting the data type.
If we only needed to manipulate the values of our expression type to interpret values of that type, then this lets us skip having to define the type and create values of that types entirely.

For the base case we want to be able to produce "things" from integer literals and from pairs of "things", so we write:
```haskell
class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr
```

Our interpreters will be instances of this typeclass.

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

We can also add constructors to our virtual data type by writing addition typeclass:
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
This happens because we are passing typeclass dictionaries all over the place, although this may not actually effect you if your code is simple and/or if you compile with optimisations turned up.

## Final encoding with Backpack

We're now going to solve the problem again using Backpack, partly to address these problems and partly just to play around with Backpack a little.
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
although we won't be able to use them until we supply an implementation for `Repr`.

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

## An initial encoding that isn't extendable

If we really want to be able to play with values of our term, we can write a data type for our terms:
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

## Initial encoding with classy `Prism`s

```haskell
newtype Term f = Term { unTerm :: f (Term f) }

makeWrapped ''Term
```

```haskell
data BaseF f =
    TmLit !Int
  | TmAdd !f !f
  deriving (Eq, Ord, Show)

makePrisms ''BaseF
```

```haskell
class HasBaseF tm where
  _BaseF :: Prism' (tm f) (BaseF f)

  _Lit :: Prism' (Term tm) Int
  _Lit = _Wrapped . _BaseF . _TmLit
  {-# INLINE _Lit #-}

  _Add :: Prism' (Term tm) (Term tm, Term tm)
  _Add = _Wrapped . _BaseF . _TmAdd
  {-# INLINE _Add #-}

instance HasBaseF BaseF where
  _BaseF = id
  {-# INLINE _BaseF #-}
```

```haskell
lit :: HasBaseF tm => Int -> Term tm
lit = review _Lit

add :: HasBaseF tm => Term tm -> Term tm -> Term tm
add tm1 tm2 = review _Add (tm1, tm2)
```

```haskell
addRule :: HasBaseF tm => EvalRule (Term tm)
addRule = EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Add tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 + i2)
```

```haskell
data MulF f =
    TmMul !f !f
  deriving (Eq, Ord, Show)

makePrisms ''MulF
```

```haskell
class HasMulF tm where
  _MulF :: Prism' (tm f) (MulF f)

  _Mul :: Prism' (Term tm) (Term tm, Term tm)
  _Mul = _Wrapped . _MulF . _TmMul
  {-# INLINE _Mul #-}

instance HasMulF MulF where
  _MulF = id
  {-# INLINE _MulF #-}
```

```haskell
mul :: HasMulF tm => Term tm -> Term tm -> Term tm
mul tm1 tm2 = review _Mul (tm1, tm2)
```

```haskell
mulRule :: (HasBaseF tm, HasMulF tm) => EvalRule (Term tm)
mulRule = EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Mul tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 * i2)
```

```haskell
data TermF f =
    BMBase !(BaseF f)
  | BMMul !(MulF f)
  deriving (Eq, Ord, Show)

makePrisms ''TermF
```

```haskell
instance HasBaseF TermF where
  _BaseF = _BMBase
  {-# INLINE _BaseF #-}
```

```haskell
instance HasMulF TermF where
  _MulF = _BMMul
  {-# INLINE _MulF #-}
```

## Initial encoding with classy `Prism`s - another approach

```haskell
class HasBase tm where
  _Lit :: Prism' (Term tm) Int
  _Add :: Prism' (Term tm) (Term tm, Term tm)
```

```haskell
class HasMul tm where
  _Mul :: Prism' (Term tm) (Term tm, Term tm)
```

```haskell
data TermF f =
    BMLit !Int
  | BMAdd !f !f
  | BMMul !f !f
  deriving (Eq, Ord, Show)

makePrisms ''TermF
```

```haskell
instance HasBase TermF where
  _Lit = _Wrapped . _BMLit
  {-# INLINE _Lit #-}
  _Add = _Wrapped . _BMAdd
  {-# INLINE _Add #-}
```

```haskell
instance HasMul TermF where
  _Mul = _Wrapped . _BMMul
  {-# INLINE _Mul #-}
```

## Initial encoding with Backpack

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
    Lit !Int
  | Add !Term !Term
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
    Lit !Int
  | Add !Term !Term
  | Mul !Term !Term
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

# Benchmarking the code

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

The tagless final approach on `Int`
```
time                 4.914 ns   (4.846 ns .. 4.981 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 4.887 ns   (4.843 ns .. 4.931 ns)
std dev              147.4 ps   (128.8 ps .. 170.5 ps)
```
and on a newtype wrapping `Int`
```
time                 4.504 ns   (4.438 ns .. 4.568 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 4.518 ns   (4.470 ns .. 4.575 ns)
std dev              174.0 ps   (144.2 ps .. 222.0 ps)
```
and the version using Backpack
```
time                 4.941 ns   (4.891 ns .. 4.998 ns)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 4.999 ns   (4.936 ns .. 5.145 ns)
std dev              295.7 ps   (161.1 ps .. 593.6 ps)
```
were all very similar.

The vanilla initial encoding was slower that the final encodings
```
time                 79.14 ns   (77.89 ns .. 80.63 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 79.04 ns   (78.17 ns .. 80.08 ns)
std dev              3.335 ns   (2.678 ns .. 4.213 ns)
```
which makes sense, since it needed to build up and tear down values for the expressions.

The versions using classy `Prism`s
```
time                 78.82 ns   (77.89 ns .. 79.91 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 79.24 ns   (78.40 ns .. 80.21 ns)
std dev              3.210 ns   (2.656 ns .. 4.498 ns)
```
and Backpack
```
time                 74.67 ns   (74.20 ns .. 75.26 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 75.52 ns   (74.79 ns .. 76.56 ns)
std dev              2.982 ns   (2.371 ns .. 4.149 ns)
```
were similar to the vanilla version.

The version using classy `Prism`s via the convenience types for the various pieces:
```haskell
data TermF f =
    BMBase !(BaseF f)
  | BMMul  !(MulF f)
```
fared a bit worse:
```
time                 126.1 ns   (124.8 ns .. 127.5 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 126.8 ns   (125.4 ns .. 128.5 ns)
std dev              5.051 ns   (4.087 ns .. 6.742 ns)
```

# Looking at the core

## Final encoding

final int
```
evalAddMulBig :: Int -> Int
evalAddMulBig
  = \ (tm :: Int) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }
```

final eval
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

final bp
```
evalAddMulBig :: Repr -> Repr
evalAddMulBig
  = \ (tm :: Repr) ->
      case tm of { I# x ->
      I# (+# (*# (+# x 3#) (+# x 5#)) (*# (+# x 7#) (+# x 11#)))
      }
```

## Initial encodings

vanilla
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

initial bp
```
Rec {
-- RHS size: {terms: 39, types: 15, coercions: 0, joins: 0/0}
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

fuse
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

compose
```
Rec {
-- RHS size: {terms: 67, types: 102, coercions: 43, joins: 0/0}
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

vanilla
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

initial bp
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

fuse
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

compose
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
