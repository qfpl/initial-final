{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Term.Type where

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data Term =
    Lit Int
  | Add Term Term
  | Mul Term Term
  deriving (Eq, Ord, Show, Generic)

makePrisms ''Term

instance NFData Term
