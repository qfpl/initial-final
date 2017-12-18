{-# LANGUAGE TemplateHaskell #-}
module Term.Type where

import Control.Lens

data Term =
    Lit Int
  | Add Term Term
  deriving (Eq, Ord, Show)

makePrisms ''Term
