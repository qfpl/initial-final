module Base where

import Data.Functor.Identity

lit :: a -> Identity a
lit = Identity

add :: Num a => Identity a -> Identity a -> Identity a
add x y = (+) <$> x <*> y
