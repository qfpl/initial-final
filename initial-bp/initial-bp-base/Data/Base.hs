module Data.Base where

import Data.Term.Type
import Data.Base.Type

import Control.Lens

lit :: Int -> Term
lit = review _Lit

add :: Term -> Term -> Term
add x y = review _Add (x, y)
