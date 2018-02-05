module Term.Eval where

import Term.Type

import Base.Eval
import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval addRule
