module Example.Term.Base.Eval where

import Term
import Base.Eval
import Example.Term.Base.Type

import Interpret.Eval

import Example.Base

evalTerm :: Term TermF -> Term TermF
evalTerm = mkEval evalRules

eval1 :: Term TermF
eval1 = evalTerm term1
