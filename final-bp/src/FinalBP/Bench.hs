module FinalBP.Bench where

import Repr
import Base
import Mul

lit2 :: Repr
lit2 =
  lit 2

evalAddSmall :: Repr -> Repr
evalAddSmall tm =
  add tm tm

evalAddMulSmall :: Repr -> Repr
evalAddMulSmall tm =
  add (mul tm (lit 3)) (lit 5)

evalAddBig :: Repr -> Repr
evalAddBig tm =
  add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: Repr -> Repr
evalAddMulBig tm =
  add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
