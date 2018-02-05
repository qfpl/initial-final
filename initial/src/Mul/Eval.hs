module Mul.Eval where

import Control.Lens

import Term
import Base.Type
import Mul.Type
import Interpret.Eval

mulRule :: (HasBaseF f, HasMulF f) => EvalRule (Term f a)
mulRule =
  let
    mulEval e tm = do
      (tm1, tm2) <- preview _Mul tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 * i2)
  in
    EvalRule $ \e good bad tm ->
      maybe (bad tm) good . mulEval e $ tm

evalRules :: (HasBaseF f, HasMulF f) => [EvalRule (Term f a)]
evalRules =
    [ mulRule ]
