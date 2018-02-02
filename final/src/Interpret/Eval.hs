{-# LANGUAGE DeriveGeneric #-}
module Interpret.Eval (
    Eval(..)
  ) where

import GHC.Generics
import Control.DeepSeq

import Base
import Mul

newtype Eval = Eval { runEval :: Int }
  deriving (Eq, Ord, Show, Generic)

instance NFData Eval

instance ExpBase Eval where
  lit = Eval
  add (Eval x) (Eval y) = Eval (x + y)

instance ExpMul Eval where
  mul (Eval x) (Eval y) = Eval (x * y)
