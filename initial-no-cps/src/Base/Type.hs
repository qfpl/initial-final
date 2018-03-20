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

class HasBaseF tm where
  _BaseF :: Prism' (tm f) (BaseF f)

  _Lit :: Prism' (Term tm) Int
  _Lit = _Wrapped . _BaseF . _TmLit
  {-# INLINE _Lit #-}

  _Add :: Prism' (Term tm) (Term tm, Term tm)
  _Add = _Wrapped . _BaseF . _TmAdd
  {-# INLINE _Add #-}

instance HasBaseF BaseF where
  _BaseF = id
  {-# INLINE _BaseF #-}

