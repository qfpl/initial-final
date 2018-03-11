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

class HasMul tm where
  _Mul :: Prism' (Term tm a) (Term tm a, Term tm a)

instance HasMul MulF where
  _Mul = _Wrapped . _TmMul
  {-# INLINE _Mul #-}



