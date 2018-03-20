module FinalGADT.Bench where

import Repr
import Base
import Mul

lit2 :: Repr Int
lit2 =
  lit 2

evalAddSmall :: Repr Int -> Repr Int
evalAddSmall tm =
  add tm tm

evalAddMulSmall :: Repr Int -> Repr Int
evalAddMulSmall tm =
  add (mul tm (lit 3)) (lit 5)

evalAddBig :: Repr Int -> Repr Int
evalAddBig tm =
  add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: Repr Int -> Repr Int
evalAddMulBig tm =
  add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
