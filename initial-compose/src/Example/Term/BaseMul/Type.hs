{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Term.BaseMul.Type where

import Base.Type
import Mul.Type

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data TermF f =
    BMBase !(BaseF f)
  | BMMul !(MulF f)
  deriving (Eq, Ord, Show, Generic)

makePrisms ''TermF

instance NFData f => NFData (TermF f)

instance HasBaseF TermF where
  _BaseF = _BMBase
  {-# INLINE _BaseF #-}

instance HasMulF TermF where
  _MulF = _BMMul
  {-# INLINE _MulF #-}
