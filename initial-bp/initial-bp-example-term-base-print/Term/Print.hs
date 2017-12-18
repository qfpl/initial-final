module Term.Print where

import Term.Type

import Base.Print
import Interpret.Print

printTerm :: Term -> Maybe String
printTerm = mkPrint printRules
