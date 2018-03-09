{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Final.Bench.Int where

import Base
import Mul

lit2 :: Int
lit2 =
  lit 2

evalAddSmall :: Int -> Int
evalAddSmall tm =
  add tm tm

evalAddMulSmall :: Int -> Int
evalAddMulSmall tm =
  add (mul tm (lit 3)) (lit 5)

evalAddBig :: Int -> Int
evalAddBig tm =
  add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: Int -> Int
evalAddMulBig tm =
  add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
