module Data.Term.Eval where

import Data.Term.Type

import Data.Base.Eval
import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval evalRules
