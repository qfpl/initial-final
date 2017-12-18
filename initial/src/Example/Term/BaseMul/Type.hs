{-# LANGUAGE TemplateHaskell #-}
module Example.Term.BaseMul.Type where

import Base.Type
import Mul.Type

import Control.Lens

data TermF f a = BMBase (BaseF f a) | BMMul (MulF f a)
  deriving (Eq, Ord, Show)

makePrisms ''TermF

instance HasBaseF TermF where
  _BaseF = _BMBase

instance HasMulF TermF where
  _MulF = _BMMul
