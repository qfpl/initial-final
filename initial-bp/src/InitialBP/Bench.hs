module InitialBP.Bench where

import Term.Type

import qualified Base.Eval as BE
import qualified Mul.Eval as ME

import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval $ mappend BE.evalRules ME.evalRules

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
