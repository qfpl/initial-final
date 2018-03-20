module Mul where

import Data.Functor.Identity

mul :: Num a => Identity a -> Identity a -> Identity a
mul x y = (*) <$> x <*> y
