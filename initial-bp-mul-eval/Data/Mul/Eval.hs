module Data.Mul.Eval where

import Data.Term.Type
import Data.Base.Type
import Data.Mul.Type

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
