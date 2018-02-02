{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Term.BaseMul.Type where

import Base.Type
import Mul.Type

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data TermF f a =
    BMBase (BaseF f a)
  | BMMul (MulF f a)
  deriving (Eq, Ord, Show, Generic)

makePrisms ''TermF

instance NFData (f a) => NFData (TermF f a)

instance HasBaseF TermF where
  _BaseF = _BMBase

instance HasMulF TermF where
  _MulF = _BMMul
