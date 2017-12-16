module Data.Base.Eval where

import Data.Term.Type
import Data.Base.Type

import Interpret.Eval

import Control.Lens

evalRules :: [EvalRule Term]
evalRules =
  let
    litRule _ tm = do
      i <- preview _Lit tm
      pure $  review _Lit i
    addRule e tm = do
      (tm1, tm2) <- preview _Add tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 + i2)
  in
    [ EvalRule litRule
    , EvalRule addRule
    ]
