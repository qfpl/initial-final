module Data.Term.Print where

import Data.Term.Type

import qualified Data.Base.Print as BP
import qualified Data.Mul.Print as MP
import Interpret.Print

printTerm :: Term -> Maybe String
printTerm = mkPrint $ BP.printRules ++ MP.printRules
