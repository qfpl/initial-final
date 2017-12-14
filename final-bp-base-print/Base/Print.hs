{-# LANGUAGE TypeApplications #-}
module Base.Print where

import Base
import Print

lit = show @Int
add x y = "(" ++ x ++ ") + (" ++ y ++ ")"
