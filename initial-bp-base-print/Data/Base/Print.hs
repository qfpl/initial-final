module Data.Base.Print where

import Data.Term.Type
import Data.Base.Type

import Interpret.Print

import Control.Lens

printRules :: [PrintRule Term]
printRules =
  let
    litPrint _ tm = do
      tm' <- preview _Lit tm
      pure $ show tm'
    addPrint pr tm = do
      (tm1, tm2) <- preview _Add tm
      pr1 <- pr tm1
      pr2 <- pr tm2
      pure $ "(" ++ pr1 ++ " + " ++ pr2 ++ ")"
  in
    [ PrintRule litPrint
    , PrintRule addPrint
    ]
