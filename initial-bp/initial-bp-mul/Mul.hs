module Mul where

import Term.Type
import Mul.Type

import Control.Lens

mul :: Term -> Term -> Term
mul x y = review _Mul (x, y)
