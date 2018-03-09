{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mul (
    ExpMul(..)
  ) where

class ExpMul repr where
  mul :: repr -> repr -> repr

instance ExpMul Int where
  mul = (*)

instance ExpMul String where
  mul x y = mconcat ["(", x, " * ", y, ")"]

