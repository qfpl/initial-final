{-# LANGUAGE TemplateHaskell #-}
module Data.Base (
    BaseF(..)
  , HasBaseF(..)
  , lit
  , add
  , baseEvalRules
  , basePrintRules
  ) where

import Control.Lens

import Data.Term
import Interpret.Eval
import Interpret.Print

data BaseF f a =
    TmLit Int
  | TmAdd (f a) (f a)
  deriving (Eq, Ord, Show)

makePrisms ''BaseF

class HasBaseF tm where
  _BaseF :: Prism' (tm f a) (BaseF f a)

  _Lit :: Prism' (Term tm a) Int
  _Lit = _Wrapped . _BaseF . _TmLit

  _Add :: Prism' (Term tm a) (Term tm a, Term tm a)
  _Add = _Wrapped . _BaseF . _TmAdd

lit :: HasBaseF tm => Int -> Term tm a
lit = review _Lit

add :: HasBaseF tm => Term tm a -> Term tm a -> Term tm a
add tm1 tm2 = review _Add (tm1, tm2)

instance HasBaseF BaseF where
  _BaseF = id

baseEvalRules :: HasBaseF f => [EvalRule (Term f a)]
baseEvalRules =
  let
    litRule _ tm = do
      i <- preview _Lit tm
      pure $  review _Lit i
    addRule e tm = do
      (tm1, tm2) <- preview _Add tm
      i1 <- preview _Lit (e tm1)
      i2 <- preview _Lit (e tm2)
      pure $ review _Lit (i1 + i2)
  in
    [ EvalRule litRule
    , EvalRule addRule
    ]

basePrintRules :: HasBaseF f => [PrintRule (Term f a)]
basePrintRules =
  let
    litPrint _ tm = do
      tm' <- preview _Lit tm
      pure $ show tm'
    addPrint pr tm = do
      (tm1, tm2) <- preview _Add tm
      pr1 <- pr tm1
      pr2 <- pr tm2
      pure $ "(" ++ pr1 ++ " + " ++ pr2 ++ ")"
  in
    [ PrintRule litPrint
    , PrintRule addPrint
    ]
