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

class HasMulF tm where
  _MulF :: Prism' (tm f) (MulF f)

  _Mul :: Prism' (Term tm) (Term tm, Term tm)
  _Mul = _Wrapped . _MulF . _TmMul
  {-# INLINE _Mul #-}

instance HasMulF MulF where
  _MulF = id
  {-# INLINE _MulF #-}



