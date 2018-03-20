{-# LANGUAGE RankNTypes #-}
module Interpret.Eval (
    EvalRule(..)
  , mkEval
  ) where

import Data.Foldable (asum)

newtype EvalRule tm =
  EvalRule ((tm -> tm) -> tm -> Maybe tm)

mkEval :: [EvalRule tm] -> tm -> tm
mkEval rules =
  let
    step tm = asum . fmap (\(EvalRule f) -> f eval tm) $ rules
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
