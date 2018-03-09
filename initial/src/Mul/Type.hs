{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Mul.Type where

import Term

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data MulF f a =
    TmMul !(f a) !(f a)
  deriving (Eq, Ord, Show, Generic)

makePrisms ''MulF

instance NFData (f a) => NFData (MulF f a)

class HasMulF tm where
  _MulF :: Prism' (tm f a) (MulF f  a)

instance HasMulF MulF where
  _MulF = id

_Mul :: HasMulF tm => Prism' (Term tm a) (Term tm a, Term tm a)
_Mul = _Wrapped . _MulF . _TmMul

