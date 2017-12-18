{-# LANGUAGE TemplateHaskell #-}
module Mul (
    MulF(..)
  , HasMulF(..)
  , mul
  , mulEvalRules
  , mulPrintRules
  , BaseMulF(..)
  ) where

import Control.Lens

import Term
import Base
import Interpret.Eval
import Interpret.Print

data MulF f a =
    TmMul (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''MulF

class HasMulF tm where
  _MulF :: Prism' (tm f a) (MulF f  a)

  _Mul :: Prism' (Term tm a) (Term tm a, Term tm a)
  _Mul = _Wrapped . _MulF . _TmMul

mul :: HasMulF tm => Term tm a -> Term tm a -> Term tm a
mul tm1 tm2 = review _Mul (tm1, tm2)

instance HasMulF MulF where
  _MulF = id

mulEvalRules :: (HasBaseF f, HasMulF f) => [EvalRule (Term f a)]
mulEvalRules =
  let
    mulEval e tm = do
      (tm1, tm2) <- preview _Mul tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 * i2)
  in
    [ EvalRule mulEval ]

mulPrintRules :: HasMulF f => [PrintRule (Term f a)]
mulPrintRules =
  let
    mulPrint p tm = do
      (tm1, tm2) <- preview _Mul tm
      pr1 <- p tm1
      pr2 <- p tm2
      pure $ "(" ++ pr1 ++ " * " ++ pr2 ++ ")"
  in
    [ PrintRule mulPrint ]

data BaseMulF f a = BMBase (BaseF f a) | BMMul (MulF f a)
  deriving (Eq, Ord, Show)

makePrisms ''BaseMulF

instance HasBaseF BaseMulF where
  _BaseF = _BMBase

instance HasMulF BaseMulF where
  _MulF = _BMMul

