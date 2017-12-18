module Base (
    ExpBase(..)
  ) where

class ExpBase repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr
