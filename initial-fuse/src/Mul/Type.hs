{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Mul.Type where

import Term

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data MulF f =
    TmMul !f !f
  deriving (Eq, Ord, Show, Generic)

makePrisms ''MulF

instance NFData f => NFData (MulF f)

class HasMul tm where
  _Mul :: Prism' (Term tm) (Term tm, Term tm)

instance HasMul MulF where
  _Mul = _Wrapped . _TmMul
  {-# INLINE _Mul #-}



