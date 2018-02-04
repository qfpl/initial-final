{-# LANGUAGE FlexibleInstances #-}
module Vanilla.Bench where

import Term
import Interpret.Eval

lit2 :: Term
lit2 =
  Lit 2

evalAddSmall :: Term -> Term
evalAddSmall tm =
  evalTerm $ Add tm tm

evalAddMulSmall :: Term -> Term
evalAddMulSmall tm =
  evalTerm $ Add (Mul tm (Lit 3)) (Lit 5)

evalAddBig :: Term -> Term
evalAddBig tm =
  evalTerm $ Add (Add tm (Lit 3)) (Add tm (Lit 5))

evalAddMulBig :: Term -> Term
evalAddMulBig tm =
  evalTerm $ Add (Mul (Add tm (Lit 3)) (Add tm (Lit 5))) (Mul (Add tm (Lit 7)) (Add tm (Lit 11)))
