{-# LANGUAGE TemplateHaskell #-}
module Base.Type where

import Term

import Control.Lens

data BaseF f a =
    TmLit Int
  | TmAdd (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''BaseF

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
