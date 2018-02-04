module Interpret.Print (
    printTerm
  ) where

import Term

printTerm :: Term -> String
printTerm (Lit i) = 
  show i
printTerm (Add tm1 tm2) = 
  "(" ++ printTerm tm1 ++ " + " ++ printTerm tm2 ++ ")"
printTerm (Mul tm1 tm2) = 
  "(" ++ printTerm tm1 ++ " * " ++ printTerm tm2 ++ ")"
