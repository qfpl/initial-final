{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Base.Type where

import Term

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

data BaseF f =
    TmLit !Int
  | TmAdd !f !f
  deriving (Eq, Ord, Show, Generic)

makePrisms ''BaseF

instance NFData f => NFData (BaseF f)

class HasBase tm where
  _Lit :: Prism' (Term tm) Int
  _Add :: Prism' (Term tm) (Term tm, Term tm)

instance HasBase BaseF where
  _Lit = _Wrapped . _TmLit
  {-# INLINE _Lit #-}

  _Add = _Wrapped . _TmAdd
  {-# INLINE _Add #-}

