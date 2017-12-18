module Term.Print where

import Term.Type

import qualified Base.Print as BP
import qualified Mul.Print as MP
import Interpret.Print

printTerm :: Term -> Maybe String
printTerm = mkPrint $ BP.printRules ++ MP.printRules
