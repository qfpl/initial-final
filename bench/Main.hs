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
main = defaultMain [
    bgroup "add - small" [
      bench "vanilla" $ nf Vanilla.evalAddSmall Vanilla.lit2
    , bench "final (Int)" $ nf FinalInt.evalAddSmall FinalInt.lit2
    , bench "final (Eval)" $ nf FinalEval.evalAddSmall FinalEval.lit2
    , bench "final-bp" $ nf FinalBP.evalAddSmall FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddSmall Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddSmall InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add and mul - small" [
      bench "vanilla" $ nf Vanilla.evalAddMulSmall Vanilla.lit2
    , bench "final (Int)" $ nf FinalInt.evalAddMulSmall FinalInt.lit2
    , bench "final (Eval)" $ nf FinalEval.evalAddMulSmall FinalEval.lit2
    , bench "final-bp" $ nf FinalBP.evalAddMulSmall FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddMulSmall Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddMulSmall InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add - big" [
      bench "vanilla" $ nf Vanilla.evalAddBig Vanilla.lit2
    , bench "final (Int)" $ nf FinalInt.evalAddBig FinalInt.lit2
    , bench "final (Eval)" $ nf FinalEval.evalAddBig FinalEval.lit2
    , bench "final-bp" $ nf FinalBP.evalAddBig FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddBig Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddBig InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add and mul - big" [
      bench "vanilla" $ nf Vanilla.evalAddMulBig Vanilla.lit2
    , bench "final (Int)" $ nf FinalInt.evalAddMulBig FinalInt.lit2
    , bench "final (Eval)" $ nf FinalEval.evalAddMulBig FinalEval.lit2
    , bench "final-bp" $ nf FinalBP.evalAddMulBig FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddMulBig Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddMulBig InitialBP.evalTerm) InitialBP.lit2
    ]
  ]
