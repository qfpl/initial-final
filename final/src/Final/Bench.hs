module Final.Bench where

import Base
import Mul
import Interpret.Eval

lit2 :: Eval
lit2 =
  lit 2

evalAddSmall :: Eval -> Eval
evalAddSmall tm =
  add tm tm

evalAddMulSmall :: Eval -> Eval
evalAddMulSmall tm =
  add (mul tm (lit 3)) (lit 5)

evalAddBig :: Eval -> Eval
evalAddBig tm =
  add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: Eval -> Eval
evalAddMulBig tm =
  add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
