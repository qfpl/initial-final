module Example.Mul where

import Repr
import Base
import Mul
import Example.Base

term2 :: Num a => Repr a
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Num a => Repr a
term3 = mul (lit 7) term1
