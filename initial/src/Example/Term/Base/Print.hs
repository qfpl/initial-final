module Example.Term.Base.Print where

import Term
import Base.Print
import Example.Term.Base.Type

import Interpret.Print

import Example.Base

printTerm :: Term TermF a -> Maybe String
printTerm = mkPrint printRules

print1 :: Maybe String
print1 = printTerm term1
