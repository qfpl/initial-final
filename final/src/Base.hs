{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Base (
    ExpBase(..)
  ) where

class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr

instance ExpBase Int where
  lit = id
  add = (+)

instance ExpBase String where
  lit = show
  add x y = mconcat ["(", x, " + ", y, ")"]
