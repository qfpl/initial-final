module Mul.Eval where

import Term.Type
import Base.Type
import Mul.Type

import Interpret.Eval

import Control.Lens

mulRule :: EvalRuleK Term
mulRule = toEvalK . EvalRule $ \e tm -> do
  (tm1, tm2) <- preview _Mul tm
  i1 <- preview _Lit (e tm1)
  i2 <- preview _Lit (e tm2)
  pure $ review _Lit (i1 * i2)

evalRules :: EvalRuleK Term
evalRules = mulRule
{-# INLINE evalRules #-}
