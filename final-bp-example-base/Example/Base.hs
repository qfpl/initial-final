module Example.Base where

import Final
import Base

term1 :: Repr
term1 = add (lit 8) (add (lit 1) (lit 2))
