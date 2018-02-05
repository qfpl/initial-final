{-# LANGUAGE FlexibleInstances #-}
module Initial.Bench where

import Control.DeepSeq (NFData)

import Term
import qualified Base.Eval as B
import qualified Mul.Eval as M
import Example.Term.BaseMul.Type

import Interpret.Eval
import Base
import Mul

instance NFData (Term TermF a)

evalTerm :: Term TermF a -> Term TermF a
evalTerm =
  mkEval (composeEvalRule B.addRule M.mulRule)

lit2 :: Term TermF a
lit2 =
  lit 2

evalAddSmall :: (Term TermF a -> Term TermF a) -> Term TermF a -> Term TermF a
evalAddSmall eval tm =
  eval $ add tm tm

evalAddMulSmall :: (Term TermF a -> Term TermF a) -> Term TermF a -> Term TermF a
evalAddMulSmall eval tm =
  eval $ add (mul tm (lit 3)) (lit 5)

evalAddBig :: (Term TermF a -> Term TermF a) -> Term TermF a -> Term TermF a
evalAddBig eval tm =
  eval $ add (add tm (lit 3)) (add tm (lit 5))

evalAddMulBig :: (Term TermF a -> Term TermF a) -> Term TermF a -> Term TermF a
evalAddMulBig eval tm =
  eval $ add (mul (add tm (lit 3)) (add tm (lit 5))) (mul (add tm (lit 7)) (add tm (lit 11)))
