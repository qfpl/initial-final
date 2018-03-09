{-# LANGUAGE DeriveGeneric #-}
module Term (
    Term(..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics

data Term =
    Lit !Int
  | Add !Term !Term
  | Mul !Term !Term
  deriving (Eq, Show, Generic)

instance NFData Term


