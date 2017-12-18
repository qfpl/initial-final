module Data.Mul where

import Data.Term.Type
import Data.Mul.Type

import Control.Lens

mul :: Term -> Term -> Term
mul x y = review _Mul (x, y)
