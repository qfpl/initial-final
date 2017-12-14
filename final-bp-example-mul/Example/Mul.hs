module Example.Mul where

import Final
import Base
import Mul
import Example.Base

term2 :: Repr
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Repr
term3 = mul (lit 7) term1
