module Example.Base where

import Term
import Base
import Base.Type

term1 :: HasBase tm => Term tm
term1 = add (lit 8) (add (lit 1) (lit 2))
