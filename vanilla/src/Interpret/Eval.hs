module Interpret.Eval (
    evalTerm
  ) where

import Term

evalTerm :: Term -> Term
evalTerm tm@(Add tm1 tm2) =
  case evalTerm tm1 of
    Lit i1 ->
      case evalTerm tm2 of
        Lit i2 -> Lit (i1 + i2)
        _ -> tm
    _ -> tm
evalTerm tm@(Mul tm1 tm2) =
  case evalTerm tm1 of
    Lit i1 ->
      case evalTerm tm2 of
        Lit i2 -> Lit (i1 * i2)
        _ -> tm
    _ -> tm
evalTerm tm =
  tm
