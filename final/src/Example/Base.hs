module Example.Base where

import Base

term1 :: ExpBase repr => repr
term1 = add (lit 8) (add (lit 1) (lit 2))
