module Example.Mul where

import Term
import Base
import Base.Type
import Mul
import Mul.Type

import Example.Base

term2 :: (HasBase tm, HasMul tm) => Term tm
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: (HasBase tm, HasMul tm) => Term tm
term3 = mul (lit 7) term1
