module Interpret.Print (
    Print(..)
  ) where

import Base
import Mul

newtype Print = Print { runPrint :: String }
  deriving (Eq, Ord, Show)

instance ExpBase Print where
  lit = Print . show
  add (Print x) (Print y) = Print ("(" ++ x ++ ") + (" ++  y ++ ")")

instance ExpMul Print where
  mul (Print x) (Print y) = Print ("(" ++ x ++ ") * (" ++  y ++ ")")
