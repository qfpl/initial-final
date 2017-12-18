module Example.Term.Base.Eval where

import Term
import Base.Eval
import Example.Term.Base.Type

import Interpret.Eval

import Example.Base

evalTerm :: Term TermF a -> Term TermF a
evalTerm = mkEval evalRules

eval1 :: Term TermF a
eval1 = evalTerm term1
