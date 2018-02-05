module Interpret.Eval (
    evalTerm
  ) where

import Term

evalTerm :: Term -> Term
evalTerm tm@(Add tm1 tm2) =
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 + i2)
    _ -> tm
evalTerm tm@(Mul tm1 tm2) =
  case (evalTerm tm1, evalTerm tm2) of
    (Lit i1, Lit i2) -> Lit (i1 * i2)
    _ -> tm
evalTerm tm =
  tm
