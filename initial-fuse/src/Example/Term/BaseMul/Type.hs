{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Term.BaseMul.Type where

import Base.Type
import Mul.Type

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data TermF f a =
    BMLit !Int
  | BMAdd !(f a) !(f a)
  | BMMul !(f a) !(f a)
  deriving (Eq, Ord, Show, Generic)

makePrisms ''TermF

instance NFData (f a) => NFData (TermF f a)

instance HasBase TermF where
  _Lit = _Wrapped . _BMLit
  {-# INLINE _Lit #-}
  _Add = _Wrapped . _BMAdd
  {-# INLINE _Add #-}

instance HasMul TermF where
  _Mul = _Wrapped . _BMMul
  {-# INLINE _Mul #-}
