{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Final.Bench.Eval where

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
