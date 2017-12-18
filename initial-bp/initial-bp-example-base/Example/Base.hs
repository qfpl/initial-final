module Example.Base where

import Term.Type
import Base

term1 :: Term
term1 = add (lit 8) (add (lit 1) (lit 2))
