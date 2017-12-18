module Mul.Print where

import Term
import Mul.Type
import Interpret.Print

import Control.Lens

printRules :: HasMulF f => [PrintRule (Term f a)]
printRules =
  let
    mulPrint p tm = do
      (tm1, tm2) <- preview _Mul tm
      pr1 <- p tm1
      pr2 <- p tm2
      pure $ "(" ++ pr1 ++ " * " ++ pr2 ++ ")"
  in
    [ PrintRule mulPrint ]
