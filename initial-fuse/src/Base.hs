{-# LANGUAGE TemplateHaskell #-}
module Base (
    lit
  , add
  ) where

import Term
import Base.Type

import Control.Lens

lit :: HasBase tm => Int -> Term tm a
lit = review _Lit

add :: HasBase tm => Term tm a -> Term tm a -> Term tm a
add tm1 tm2 = review _Add (tm1, tm2)

