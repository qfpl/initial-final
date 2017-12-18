module Example.Mul where

import Data.Term.Type
import Data.Base
import Data.Mul

import Example.Base

term2 :: Term
term2 = add (lit 7) (mul (lit 1) (lit 2))

term3 :: Term
term3 = mul (lit 7) term1
