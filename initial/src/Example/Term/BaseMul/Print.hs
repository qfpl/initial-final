module Example.Term.BaseMul.Print where

import Term
import qualified Base.Print as B
import qualified Mul.Print as M
import Example.Term.BaseMul.Type

import Interpret.Print

import Example.Base
import Example.Mul

printTerm :: Term TermF a -> Maybe String
printTerm = mkPrint $ B.printRules ++ M.printRules

print1 :: Maybe String
print1 = printTerm term1

print2 :: Maybe String
print2 = printTerm term2

print3 :: Maybe String
print3 = printTerm term3
