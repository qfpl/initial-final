module Data.Term.Eval where

import Data.Term.Type

import qualified Data.Base.Eval as BE
import qualified Data.Mul.Eval as ME

import Interpret.Eval

evalTerm :: Term -> Term
evalTerm = mkEval $ BE.evalRules ++ ME.evalRules
