module Example.Term.BaseMul.Eval where

import Term
import qualified Base.Eval as B
import qualified Mul.Eval as M
import Example.Term.BaseMul.Type

import Interpret.Eval

import Example.Base
import Example.Mul

evalTerm :: Term TermF -> Term TermF
evalTerm = mkEval $ mappend B.evalRules M.evalRules

eval1 :: Term TermF
eval1 = evalTerm term1

eval2 :: Term TermF
eval2 = evalTerm term2

eval3 :: Term TermF
eval3 = evalTerm term3
