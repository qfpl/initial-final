module Example.Term.BaseMul.Eval where

import Term
import qualified Base.Eval as B
import qualified Mul.Eval as M
import Example.Term.BaseMul.Type

import Interpret.Eval

import Example.Base
import Example.Mul

evalTerm :: Term TermF a -> Term TermF a
evalTerm = mkEval $ B.evalRules ++ M.evalRules

eval1 :: Term TermF a
eval1 = evalTerm term1

eval2 :: Term TermF a
eval2 = evalTerm term2

eval3 :: Term TermF a
eval3 = evalTerm term3
