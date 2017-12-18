module Mul (
    ExpMul(..)
  ) where

class ExpMul repr where
  mul :: repr -> repr -> repr

