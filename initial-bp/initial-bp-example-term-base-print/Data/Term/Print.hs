module Data.Term.Print where

import Data.Term.Type

import Data.Base.Print
import Interpret.Print

printTerm :: Term -> Maybe String
printTerm = mkPrint printRules
