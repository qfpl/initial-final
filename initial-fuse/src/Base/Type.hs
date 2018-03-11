{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Base.Type where

import Term

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data BaseF f a =
    TmLit !Int
  | TmAdd !(f a) !(f a)
  deriving (Eq, Ord, Show, Generic)

makePrisms ''BaseF

instance NFData (f a) => NFData (BaseF f a)

class HasBase tm where
  _Lit :: Prism' (Term tm a) Int
  _Add :: Prism' (Term tm a) (Term tm a, Term tm a)

instance HasBase BaseF where
  _Lit = _Wrapped . _TmLit
  {-# INLINE _Lit #-}

  _Add = _Wrapped . _TmAdd
  {-# INLINE _Add #-}

