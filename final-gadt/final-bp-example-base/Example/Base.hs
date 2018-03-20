module Example.Base where

import Repr
import Base

term1 :: Num a => Repr a
term1 = add (lit 8) (add (lit 1) (lit 2))
