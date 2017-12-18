module Interpret.Eval (
    Eval(..)
  ) where

import Base
import Mul

newtype Eval = Eval { runEval :: Int }

instance ExpBase Eval where
  lit = Eval
  add (Eval x) (Eval y) = Eval (x + y)

instance ExpMul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)
