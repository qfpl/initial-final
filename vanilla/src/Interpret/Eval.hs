module Interpret.Eval (
    evalTerm
  ) where

import Term

evalTerm :: Term -> Term 
evalTerm (Add tm1 tm2) = 
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 + i2)
    _ -> Add tm1 tm2
evalTerm (Mul tm1 tm2) = 
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 * i2)
    _ -> Mul tm1 tm2
evalTerm tm = 
  tm
