{-# LANGUAGE RankNTypes #-}
module Interpret.Eval (
    EvalRule(..)
  , EvalRuleK(..)
  , toEvalK
  , mkEval
  ) where

newtype EvalRule tm =
  EvalRule ((tm -> tm) -> tm -> Maybe tm)

newtype EvalRuleK tm =
  EvalRuleK (forall r. (tm -> tm) -> (tm -> r) -> (tm -> r) -> tm -> r)

toEvalK :: EvalRule tm -> EvalRuleK tm
toEvalK (EvalRule f) =
  EvalRuleK $ \e good bad tm ->
    maybe (bad tm) good . f e $ tm
{-# INLINE toEvalK #-}

instance Monoid (EvalRuleK tm) where
  mempty =
    EvalRuleK $ \_ _ bad -> bad
  {-# INLINE mempty #-}
  mappend (EvalRuleK r1) (EvalRuleK r2) =
    EvalRuleK $ \e good bad -> r1 e good (r2 e good bad)
  {-# INLINE mappend #-}

mkEval :: EvalRuleK tm -> tm -> tm
mkEval (EvalRuleK f) =
  let
    step = f eval Just (const Nothing)
    eval tm = case step tm of
      Nothing -> tm
      Just tm' -> eval tm'
  in
    eval
