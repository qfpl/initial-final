module Mul (
    mul
  ) where

import Term
import Mul.Type

import Control.Lens

mul :: HasMulF tm => Term tm -> Term tm -> Term tm
mul tm1 tm2 = review _Mul (tm1, tm2)


