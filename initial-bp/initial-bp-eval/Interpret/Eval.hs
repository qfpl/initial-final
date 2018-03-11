{-# LANGUAGE RankNTypes #-}
module Interpret.Eval (
    EvalRule(..)
  , mkEval
  ) where

newtype EvalRule tm =
  EvalRule (forall r. (tm -> tm) -> (tm -> r) -> (tm -> r) -> tm -> r)

instance Monoid (EvalRule tm) where
  mempty =
    EvalRule $ \_ _ bad -> bad
  {-# INLINE mempty #-}
  mappend (EvalRule r1) (EvalRule r2) =
    EvalRule $ \e good bad -> r1 e good (r2 e good bad)
  {-# INLINE mappend #-}

mkEval :: EvalRule tm -> tm -> tm
mkEval (EvalRule f) =
  let
    step = f eval Just (const Nothing)
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
