{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main (
    main
  ) where

import qualified Vanilla.Bench as Vanilla
import qualified Initial.Bench as Initial
import qualified Final.Bench.Int as FinalInt
import qualified Final.Bench.Eval as FinalEval
import qualified InitialBP.Bench as InitialBP
import qualified FinalBP.Bench as FinalBP

import Criterion.Main

main :: IO ()
main =
  let
    initialEval = let e = Initial.evalTerm  in seq e e
    initialBPEval = let e = InitialBP.evalTerm  in seq e e
  in
  defaultMain [
    bgroup "add - small" [
      bench "vanilla" $ whnf Vanilla.evalAddSmall Vanilla.lit2
    , bench "final (Int)" $ whnf FinalInt.evalAddSmall FinalInt.lit2
    , bench "final (Eval)" $ whnf FinalEval.evalAddSmall FinalEval.lit2
    , bench "final-bp" $ whnf FinalBP.evalAddSmall FinalBP.lit2
    , bench "initial" $ whnf (Initial.evalAddSmall initialEval) Initial.lit2
    , bench "initial-bp" $ whnf (InitialBP.evalAddSmall initialBPEval) InitialBP.lit2
    ]
  , bgroup "add and mul - small" [
      bench "vanilla" $ whnf Vanilla.evalAddMulSmall Vanilla.lit2
    , bench "final (Int)" $ whnf FinalInt.evalAddMulSmall FinalInt.lit2
    , bench "final (Eval)" $ whnf FinalEval.evalAddMulSmall FinalEval.lit2
    , bench "final-bp" $ whnf FinalBP.evalAddMulSmall FinalBP.lit2
    , bench "initial" $ whnf (Initial.evalAddMulSmall initialEval) Initial.lit2
    , bench "initial-bp" $ whnf (InitialBP.evalAddMulSmall initialBPEval) InitialBP.lit2
    ]
  , bgroup "add - big" [
      bench "vanilla" $ whnf Vanilla.evalAddBig Vanilla.lit2
    , bench "final (Int)" $ whnf FinalInt.evalAddBig FinalInt.lit2
    , bench "final (Eval)" $ whnf FinalEval.evalAddBig FinalEval.lit2
    , bench "final-bp" $ whnf FinalBP.evalAddBig FinalBP.lit2
    , bench "initial" $ whnf (Initial.evalAddBig initialEval) Initial.lit2
    , bench "initial-bp" $ whnf (InitialBP.evalAddBig initialBPEval) InitialBP.lit2
    ]
  , bgroup "add and mul - big" [
      bench "vanilla" $ whnf Vanilla.evalAddMulBig Vanilla.lit2
    , bench "final (Int)" $ whnf FinalInt.evalAddMulBig FinalInt.lit2
    , bench "final (Eval)" $ whnf FinalEval.evalAddMulBig FinalEval.lit2
    , bench "final-bp" $ whnf FinalBP.evalAddMulBig FinalBP.lit2
    , bench "initial" $ whnf (Initial.evalAddMulBig initialEval) Initial.lit2
    , bench "initial-bp" $ whnf (InitialBP.evalAddMulBig initialBPEval) InitialBP.lit2
    ]
  ]
