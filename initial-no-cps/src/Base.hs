{-# LANGUAGE TemplateHaskell #-}
module Base (
    lit
  , add
  ) where

import Term
import Base.Type

import Control.Lens

lit :: HasBaseF tm => Int -> Term tm
lit = review _Lit

add :: HasBaseF tm => Term tm -> Term tm -> Term tm
add tm1 tm2 = review _Add (tm1, tm2)

