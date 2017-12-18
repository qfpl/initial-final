module Example.Mul where

import Base
import Mul

import Example.Base

term2 :: (ExpBase repr, ExpMul repr) => repr
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: (ExpBase repr, ExpMul repr) => repr
term3 = mul (lit 7) term1
