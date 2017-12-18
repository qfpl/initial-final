module Base where

import Term.Type
import Base.Type

import Control.Lens

lit :: Int -> Term
lit = review _Lit

add :: Term -> Term -> Term
add x y = review _Add (x, y)
