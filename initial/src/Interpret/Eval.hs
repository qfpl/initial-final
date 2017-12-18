module Interpret.Eval (
    EvalRule(..)
  , mkEval
  ) where

import Data.Foldable (asum)

data EvalRule tm =
  EvalRule ((tm -> tm) -> tm -> Maybe tm)

fixEval :: (tm -> tm) -> tm -> EvalRule tm -> Maybe tm
fixEval eval tm (EvalRule f) = f eval tm

mkEval :: [EvalRule tm] -> tm -> tm
mkEval rules =
  let
    step tm = asum . fmap (fixEval eval tm) $ rules
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
