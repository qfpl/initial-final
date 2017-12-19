# The write up here is a work in progress, more words are on the way

# Initial and final encodings, with and without Backpack

These are a few little projects that I'm using to explore a few different approaches for creating and consuming expressions in situations where you may want to add new ways of creating or consuming the expressions in the future.

## The problem we are trying to solve

The toy problem I'm playing with is this.

Imagine that you have a data type for expressions, like:
```haskell
data Term =
    Lit Int        -- integer literals
  | Add Term Term  -- addition
```
and functions which operate over these expressions, like:
```haskell
evalTerm :: Term -> Term
evalTerm (Lit i) = Lit i
evalTerm (Add x y) = 
  case (evalTerm x, evalTerm y) of
    (Lit i, Lit j) -> Lit (i + j)
    (x', y')       -> Add x' y'
```

With some smart constructors thrown into the mix, we have values that look like:
```haskell
term1 :: Term
term1 = add (lit 8) (add (lit 1) (lit 2))
```
and that behave as we expect
```haskell
> evalTerm term1
Lit 11
```

As time goes by, we might want to add new features to our expression:
```haskell
data Term =
    Lit Int
  | Add Term Term
  | Mul Term Term  -- new case: multiplication
```
with values looking like:
```haskell
term2 :: Term 
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Term 
term3 = mul (lit 7) term1
```

We would like to be able to do this with minimal impact on any existing expressions we might have in our code, and with minimal impact on our existing functions like `evalTerm`.

Some approaches to this problem have a trade off, where we can be more flexible with being able to add things to our expressions if we limit ourselves to a fixed set of functions that operate over our expressions.

The problem with that is that we may end up wanting to add new functions in the future, like:
```haskell
printTerm :: Term -> String
printTerm (Lit i)   = show i
printTerm (Add x y) = "(" ++ printTerm x ++ ") + (" ++ printTerm y ++ ")"
```

## The final encoding with the finally tagless style

We start by defining a typeclass for our expressions:
```haskell
class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr
```
which we can use straight away:
```haskell
term1 :: ExpBase repr => repr
term1 = add (lit 8) (add (lit 1) (lit 2))
```

If we want to evaluate these terms, we can create a newtype:
```haskell
newtype Eval = Eval { runEval :: Int }
  deriving (Eq, Ord, Show)
```
and write a typeclass instance for it:
```haskell
instance ExpBase Eval where
  lit = Eval
  add (Eval x) (Eval y) = Eval (x + y)
```

We could have written `instance ExpBase Int` instead of creating a newtype elsewhere, but then we'd end up with all of our interpretations of our expression in the same module.

We can extend this expression with multiplication, by writing a new typeclass:
```haskell
class ExpMul repr where
  mul :: repr -> repr -> repr
```
and we can create terms using the combination of these typeclasses:
```haskell
term2 :: (ExpBase repr, ExpMul repr) => repr
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: (ExpBase repr, ExpMul repr) => repr
term3 = mul (lit 7) term1
```

These types can be inferred, which is nice.

We can extend the evaluation to handle multiplication by writing the appropriate instance:
```haskell
instance ExpMul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)
```

We can also come up with new interpretations, like a pretty printer:
```haskell
newtype Print = Print { runPrint :: String }
  deriving (Eq, Ord, Show)

instance ExpBase Print where
  lit = Print . show
  add (Print x) (Print y) = Print ("(" ++ x ++ ") + (" ++  y ++ ")")

instance ExpMul Print where
  mul (Print x) (Print y) = Print ("(" ++ x ++ ") * (" ++  y ++ ")")
```

It's not all ice-cream and waterslides.  We need to put the instances of these typeclasses either with the definition of the typeclass or with the definition of the type we are using as the output.

We're also passing a lot of dictionaries around to do this, so we're perhaps not as tagless as we could be.

## The final encoding with Backpack

We start out with a signature that gives us the `Repr` type:

```
library repr
  signatures:          Repr
  hs-source-dirs:      repr
```

```haskell
-- repr/Repr.hsig
signature Repr where

data Repr
```

We use that type in another signature, which gives us the first pieces of our expressions:

```
library final-bp-base
  signatures:          Base
  build-depends:       repr
  hs-source-dirs:      final-bp-base
```

```haskell
-- final-bp-base/Base.hsig
signature Base where

import Repr

lit :: Int -> Repr
add :: Repr -> Repr -> Repr
```

We can build up expressions using these, although they'll be abstract until we provide implementations for the `Repr` and `Base` signatures:

```
library final-bp-example-base
  exposed-modules:     Example.Base
  build-depends:       final-bp-base
  hs-source-dirs:      final-bp-example-base
```

```haskell
-- final-bp-example-base/Example/Base.hs
module Example.Base where

import Repr
import Base

term1 :: Repr
term1 = add (lit 8) (add (lit 1) (lit 2))
```

For evaluation, our base type will be `Int`:
```
library final-bp-eval
  exposed-modules:     Repr
  hs-source-dirs:      final-bp-eval
```

```haskell
-- final-bp-eval/Repr.hs
module Repr where

type Repr = Int
```

and the we'll interpret the pieces we have accordingly:

```
library final-bp-base-eval
  exposed-modules:     Base
  build-depends:       final-bp-eval
  hs-source-dirs:      final-bp-base-eval
```

```haskell
-- final-bp-base-eval/Base.hs
module Base where

lit :: Int -> Int
lit = id

add :: Int -> Int -> Int
add = (+)
```

At this point, we've provided implementations for all of the signatures that were in play, so we can bring our old example back into scope and we'll interpret it as an evaluation:

```
library final-bp-example-eval
  exposed-modules:     Example.Eval
  build-depends:       final-bp-eval
                     , final-bp-base-eval
                     , final-bp-example-base
  hs-source-dirs:      final-bp-example-eval
```

```haskell
-- final-bp-example-eval/Example/Eval.hs
module Example.Eval

import Example.Base

eval1 :: Int
eval1 = term1
```

We can add a signature for multiplication:

```
library final-bp-mul
  signatures:          Mul
  build-depends:       repr
  hs-source-dirs:      final-bp-mul
```

```haskell
-- final-bp-mul/Mul.hsig
signature Mul where

import Repr

mul :: Repr -> Repr -> Repr
```

and we can use that with our other pieces to build up more varied expressions:

```
library final-bp-example-mul
  import:              options, deps
  exposed-modules:     Example.Mul
  build-depends:       final-bp-base
                     , final-bp-mul
                     , final-bp-example-base
  hs-source-dirs:      final-bp-example-mul
```

```haskell
-- final-bp-example-mul/Example/Mul.hs
module Example.Mul where

import Repr
import Base
import Mul
import Example.Base

term2 :: Repr
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Repr
term3 = mul (lit 7) term1
```

We can add an interpretation for multiplication when we want to evaluate our expressions:

```
library final-bp-mul-eval
  exposed-modules:     Mul
  build-depends:       final-bp-eval
  hs-source-dirs:      final-bp-mul-eval
```

```haskell
-- final-bp-mul-eval/Mul.hs
module Mul where

mul :: Int -> Int -> Int
mul = (*)
```

which we can make use of like we did before:

```
library final-bp-example-eval
  import:              options, deps
  hs-source-dirs:      final-bp-example-eval
  exposed-modules:     Example.Eval
  build-depends:       final-bp-eval
                     , final-bp-base-eval
                     , final-bp-mul-eval
                     , final-bp-example-base
                     , final-bp-example-mul
```

```haskell
-- final-bp-example-eval/Example/Eval.hs
module Example.Eval

import Example.Base
import Example.Mul

eval1 :: Int
eval1 = term1

eval2 :: Int
eval2 = term2

eval3 :: Int
eval3 = term3
```

We can add support for pretty printing as well.

Our base type will be `String`:

```
library final-bp-print
  exposed-modules:     Repr
  hs-source-dirs:      final-bp-print
```

```haskell
module Repr where

type Repr = String
```

Which will interpret as we did before we were using Backpack, for both the base pieces:

```
library final-bp-base-print
  exposed-modules:     Base
  build-depends:       final-bp-print
  hs-source-dirs:      final-bp-base-print
```

```haskell
-- final-bp-base-print/Base.hs
module Base where

lit :: Int -> String
lit = show

add :: String -> String -> String
add x y = "(" ++ x ++ ") + (" ++ y ++ ")"
```

and for multiplication:

```
library final-bp-mul-print
  exposed-modules:     Mul
  build-depends:       final-bp-print
  hs-source-dirs:      final-bp-mul-print
```

```haskell
-- final-bp-mul-print/Mul.hs
module Mul where

mul :: String -> String -> String
mul x y = "(" ++ x ++ ") * (" ++ y ++ ")"
```

after which we can use it to interpret our example terms from earlier:

```
library final-bp-example-print
  exposed-modules:     Example.Print
  build-depends:       final-bp-print
                     , final-bp-base-print
                     , final-bp-mul-print
                     , final-bp-example-base
                     , final-bp-example-mul
  hs-source-dirs:      final-bp-example-print
```

```haskell
-- final-bp-example-print/Example/Print.hs
module Example.Print

import Example.Base
import Example.Mul

print1 :: String
print1 = term1

print2 :: String
print2 = term2

print3 :: String
print3 = term3
```

## The initial encoding with classy prisms

Sometimes we want to have a data type that we can manipulate, and that means that we are dealing with an initial encoding.

```haskell
import Control.Lens

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

instance HasBaseF BaseF where
  _BaseF = id
```

```haskell
_Lit :: HasBaseF tm => Prism' (Term tm a) Int
_Lit = _Wrapped . _BaseF . _TmLit

_Add :: HasBaseF tm => Prism' (Term tm a) (Term tm a, Term tm a)
_Add = _Wrapped . _BaseF . _TmAdd
```

```haskell
lit :: HasBaseF tm => Int -> Term tm a
lit = review _Lit

add :: HasBaseF tm => Term tm a -> Term tm a -> Term tm a
add tm1 tm2 = review _Add (tm1, tm2)
```

```haskell
term1 :: HasBaseF tm => Term tm a
term1 = add (lit 8) (add (lit 1) (lit 2))
```

```haskell
data EvalRule tm =
  EvalRule ((tm -> tm) -> tm -> Maybe tm)
```

```haskell
fixEval :: (tm -> tm) -> tm -> EvalRule tm -> Maybe tm
fixEval eval tm (EvalRule f) = f eval tm
```

```haskell
import Data.Foldable (asum)
mkEval :: [EvalRule tm] -> tm -> tm
mkEval rules =
  let
    step tm = asum . fmap (fixEval eval tm) $ rules
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
```

```haskell
evalRules :: HasBaseF f => [EvalRule (Term f a)]
evalRules =
  let
    addRule e tm = do
      (tm1, tm2) <- preview _Add tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 + i2)
  in
    [ EvalRule addRule ]
```

```haskell
evalTerm1 :: Term BaseF a
evalTerm1 =
  let
    eval = mkEval evalRules
  in
    eval term1
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

instance HasMulF MulF where
  _MulF = id
```

```haskell
_Mul :: HasMulF tm => Prism' (Term tm a) (Term tm a, Term tm a)
_Mul = _Wrapped . _MulF . _TmMul
```

```haskell
mul :: HasMulF tm => Term tm a -> Term tm a -> Term tm a
mul tm1 tm2 = review _Mul (tm1, tm2)
```

```haskell
term2 :: (HasBaseF tm, HasMulF tm) => Term tm a
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: (HasBaseF tm, HasMulF tm) => Term tm a
term3 = mul (lit 7) term1
```

```haskell
evalRules :: (HasBaseF f, HasMulF f) => [EvalRule (Term f a)]
evalRules =
  let
    mulEval e tm = do
      (tm1, tm2) <- preview _Mul tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 * i2)
  in
    [ EvalRule mulEval ]
```

```haskell
data BM f a =
    BMBase (BaseF f a)
  | BMMul (MulF f a)
  deriving (Eq, Ord, Show)

makePrisms ''BM
```

```haskell
instance HasBaseF BM where
  _BaseF = _BMBase

instance HasMulF BM where
  _MulF = _BMMul
```

```haskell
evalTerm2 :: Term BM a
evalTerm2 =
  let
    eval = mkEval (Base.evalRules ++ Mul.evalRules)
  in
    eval term2
```

## The initial encoding with Backpack

```
library initial-bp-term
  signatures:          Term.Type
  hs-source-dirs:      initial-bp-term
```

```haskell
-- initial-bp-term/Term/Type.hsig
signature Term.Type where

data Term
```

```
library initial-bp-base-sig
  signatures:          Base.Type
  build-depends:       initial-bp-term 
                     , lens
  hs-source-dirs:      initial-bp-base-sig
```

```haskell
-- initial-bp-base-sig/Base/Type.hsig
signature Base.Type where

import Term.Type

import Control.Lens

_Lit :: Prism' Term Int
_Add :: Prism' Term (Term, Term)
```

```
library initial-bp-base
  exposed-modules:     Base
  build-depends:       initial-bp-base-sig
                     , lens
  hs-source-dirs:      initial-bp-base
```

```haskell
-- inital-bp-base/Base.hs
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
library initial-bp-example-base
  exposed-modules:     Example.Base
  build-depends:       initial-bp-base
                     , lens
  hs-source-dirs:      initial-bp-example-base
```

```haskell
--- initial-bp-example-base/Example/Base.hs
module Example.Base where

import Term.Type
import Base

term1 :: Term
term1 = add (lit 8) (add (lit 1) (lit 2))
```

```
library initial-bp-example-term-base
  exposed-modules:     Term.Type
                     , Base.Type
  build-depends:       lens
  hs-source-dirs:      initial-bp-example-term-base
```

```haskell
-- initial-bp-example-term-base/Term/Type.hs
module Term.Type where

import Control.Lens

data Term =
    Lit Int
  | Add Term Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
```

```haskell
-- initial-bp-example-term-base/Base/Type.hs
module Base.Type ( _Lit, _Add) where

import Term.Type
```


```
library initial-bp-base-eval
  exposed-modules:     Base.Eval
  build-depends:       initial-bp-base-sig
                     , initial-bp-term
                     , initial-bp-eval
                     , lens
  hs-source-dirs:      initial-bp-base-eval
```

```haskell
-- initial-bp-base-eval/Base/Eval.hs
module Base.Eval where

import Term.Type
import Base.Type

import Interpret.Eval

import Control.Lens

evalRules :: [EvalRule Term]
evalRules =
  let
    addRule e tm = do
      (tm1, tm2) <- preview _Add tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 + i2)
  in
    [ EvalRule addRule ]
```

```
library initial-bp-example-term-base-eval
  exposed-modules:     Term.Eval
  build-depends:       initial-bp-example-term-base
                     , initial-bp-eval
                     , initial-bp-base-eval
  hs-source-dirs:      initial-bp-example-term-base-eval
```

```haskell
--- initial-bp-example-term-base-eval/Term/Eval.hs
module Term.Eval where

import Term.Type

import Base.Eval
import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval evalRules
```

```
library initial-bp-mul-sig
  signatures:          Mul.Type
  build-depends:       initial-bp-term 
                     , lens
  hs-source-dirs:      initial-bp-mul-sig
```

```haskell
-- inital-bp-mul-sig/Mul/Type.hsig
signature Mul.Type where

import Term.Type

import Control.Lens

_Mul :: Prism' Term (Term, Term)
```

```
library initial-bp-mul
  exposed-modules:     Mul
  build-depends:       initial-bp-mul-sig
                     , lens
  hs-source-dirs:      initial-bp-mul
```

```haskell
-- inital-bp-mul/Mul.hs
module Mul where

import Term.Type
import Mul.Type

import Control.Lens

mul :: Term -> Term -> Term
mul x y = review _Mul (x, y)
```

```
library initial-bp-mul-eval
  exposed-modules:     Mul.Eval
  build-depends:       initial-bp-term
                     , initial-bp-base-sig
                     , initial-bp-mul-sig
                     , initial-bp-eval
                     , lens
  hs-source-dirs:      initial-bp-mul-eval
```

```haskell
-- initial-bp-mul-eval/Mul/Eval.hs
module Mul.Eval where

import Term.Type
import Base.Type
import Mul.Type

import Interpret.Eval

import Control.Lens

evalRules :: [EvalRule Term]
evalRules =
  let
    mulEval e tm = do
      (tm1, tm2) <- preview _Mul tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 * i2)
  in
    [ EvalRule mulEval ]
```

```
library initial-bp-example-mul
  exposed-modules:     Example.Mul
  build-depends:       initial-bp-example-base
                     , initial-bp-base
                     , initial-bp-mul
                     , lens
  hs-source-dirs:      initial-bp-example-mul
```

```haskell
--- initial-bp-example-mul/Example/Mul.hs
module Example.Mul where

import Term.Type
import Base
import Mul

import Example.Base

term2 :: Term
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Term
term3 = mul (lit 7) term1
```

```
library initial-bp-example-term-base-mul
  exposed-modules:     Term.Type
                     , Base.Type
                     , Mul.Type
  build-depends:       lens
  hs-source-dirs:      initial-bp-example-term-base-mul
```

```haskell
-- initial-bp-example-term-base-mul/Term/Type.hs
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
-- initial-bp-example-term-base-mul/Base/Type.hs
module Base.Type ( _Lit, _Add ) where

import Term.Type
```

```haskell
-- initial-bp-example-term-base-mul/Mul/Type.hs
module Mul.Type ( _Mul ) where

import Term.Type
```

```
library initial-bp-example-term-base-mul-eval
  exposed-modules:     Term.Eval
  build-depends:       initial-bp-example-term-base-mul
                     , initial-bp-eval
                     , initial-bp-base-eval
                     , initial-bp-mul-eval
                     , lens
  hs-source-dirs:      initial-bp-example-term-base-mul-eval
```

```haskell
-- initial-bp-example-term-base-mul-eval/Term/Eval.hs
module Term.Eval where

import Term.Type

import qualified Base.Eval as BE
import qualified Mul.Eval as ME

import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval $ BE.evalRules ++ ME.evalRules
```
