module Interpret.Print (
    Print(..)
  ) where

import Class.Base
import Class.Mul

newtype Print = Print { runPrint :: String }

instance ExpBase Print where
  lit = Print . show
  add (Print x) (Print y) = Print ("(" ++ x ++ ") + (" ++  y ++ ")")

instance ExpMul Print where
  mul (Print x) (Print y) = Print ("(" ++ x ++ ") * (" ++  y ++ ")")
