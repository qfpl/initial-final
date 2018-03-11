module Mul (
    mul
  ) where

import Term
import Mul.Type

import Control.Lens

mul :: HasMul tm => Term tm a -> Term tm a -> Term tm a
mul tm1 tm2 = review _Mul (tm1, tm2)

