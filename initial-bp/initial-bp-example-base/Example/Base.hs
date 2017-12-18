module Example.Base where

import Data.Term.Type
import Data.Base

term1 :: Term
term1 = add (lit 8) (add (lit 1) (lit 2))
