{-# LANGUAGE FlexibleInstances #-}
module Initial.Bench.Fuse where

import Control.DeepSeq (NFData)

import Term
import qualified Base.Eval as B
import qualified Mul.Eval as M
import Example.Term.BaseMul.Type

import Interpret.Eval
import Base
import Mul

instance NFData (Term TermF)

evalTerm :: Term TermF -> Term TermF
evalTerm =
  mkEval $ mappend B.addRule M.mulRule

lit2 :: Term TermF
lit2 =
  lit 2

evalAddSmall :: Term TermF -> Term TermF
evalAddSmall tm =
  evalTerm $ add tm tm

evalAddMulSmall :: Term TermF -> Term TermF
evalAddMulSmall tm =
  evalTerm $ add (mul tm (lit 3)) (lit 5)

evalAddBig :: Term TermF -> Term TermF
evalAddBig tm =
  evalTerm $ add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: Term TermF -> Term TermF
evalAddMulBig tm =
  evalTerm $ add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
