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

evalAddSmall :: (Term -> Term) -> Term -> Term
evalAddSmall eval tm =
  eval $ Add tm tm

evalAddMulSmall :: (Term -> Term) -> Term -> Term
evalAddMulSmall eval tm =
  eval $ Add (Mul tm (Lit 3)) (Lit 5)

evalAddBig :: (Term -> Term) -> Term -> Term
evalAddBig eval tm =
  eval $ Add (Add tm (Lit 3)) (Add tm (Lit 5))

evalAddMulBig :: (Term -> Term) -> Term -> Term
evalAddMulBig eval tm =
  eval $ Add (Mul (Add tm (Lit 3)) (Add tm (Lit 5))) (Mul (Add tm (Lit 7)) (Add tm (Lit 11)))
