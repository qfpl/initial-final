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

class HasBaseF tm where
  _BaseF :: Prism' (tm f a) (BaseF f a)

instance HasBaseF BaseF where
  _BaseF = id
  {-# INLINE _BaseF #-}

_Lit :: HasBaseF tm => Prism' (Term tm a) Int
_Lit = _Wrapped . _BaseF . _TmLit
{-# INLINE _Lit #-}

_Add :: HasBaseF tm => Prism' (Term tm a) (Term tm a, Term tm a)
_Add = _Wrapped . _BaseF . _TmAdd
{-# INLINE _Add #-}

