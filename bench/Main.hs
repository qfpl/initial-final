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

import qualified Initial.Bench as Initial
import qualified Final.Bench as Final
import qualified InitialBP.Bench as InitialBP
import qualified FinalBP.Bench as FinalBP

import Criterion.Main

main :: IO ()
main = defaultMain [
    bgroup "add - small" [
      bench "final" $ nf Final.evalAddSmall Final.lit2
    , bench "final-bp" $ nf FinalBP.evalAddSmall FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddSmall Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddSmall InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add and mul - small" [
      bench "final" $ nf Final.evalAddMulSmall Final.lit2
    , bench "final-bp" $ nf FinalBP.evalAddMulSmall FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddMulSmall Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddMulSmall InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add - big" [
      bench "final" $ nf Final.evalAddBig Final.lit2
    , bench "final-bp" $ nf FinalBP.evalAddBig FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddBig Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddBig InitialBP.evalTerm) InitialBP.lit2
    ]
  , bgroup "add and mul - big" [
      bench "final" $ nf Final.evalAddMulBig Final.lit2
    , bench "final-bp" $ nf FinalBP.evalAddMulBig FinalBP.lit2
    , bench "initial" $ nf (Initial.evalAddMulBig Initial.evalTerm) Initial.lit2
    , bench "initial-bp" $ nf (InitialBP.evalAddMulBig InitialBP.evalTerm) InitialBP.lit2
    ]
  ]
