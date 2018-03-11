module Term.Eval where

import Term.Type

import qualified Base.Eval as BE
import qualified Mul.Eval as ME

import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval $ mappend BE.evalRules ME.evalRules
