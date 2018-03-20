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
import qualified Initial.Bench.NoCPS as InitialNoCPS
import qualified Initial.Bench.Compose as InitialCompose
import qualified Initial.Bench.Fuse as InitialFuse
import qualified Final.Bench.Int as FinalInt
import qualified Final.Bench.Eval as FinalEval
import qualified FinalGADT.Bench as FinalGADT
import qualified InitialBP.Bench as InitialBP
import qualified FinalBP.Bench as FinalBP

import Criterion.Main

main :: IO ()
main =
  let
    vl = let l = Vanilla.lit2 in seq l l
    fil = let l = FinalInt.lit2 in seq l l
    fel = let l = FinalEval.lit2 in seq l l
    fgl = let l = FinalGADT.lit2 in seq l l
    fbpl = let l = FinalBP.lit2 in seq l l
    inl = let l = InitialNoCPS.lit2 in seq l l
    icl = let l = InitialCompose.lit2 in seq l l
    ifl = let l = InitialFuse.lit2 in seq l l
    ibpl = let l = InitialBP.lit2 in seq l l
  in
  defaultMain [
    bgroup "add - small" [
      bench "vanilla" $ 
        nf Vanilla.evalAddSmall vl
    , bench "final (int)" $ 
        nf FinalInt.evalAddSmall fil
    , bench "final (eval)" $ 
        nf FinalEval.evalAddSmall fel
    , bench "final-gadt" $ 
        nf FinalGADT.evalAddSmall fgl
    , bench "final-bp" $ 
        nf FinalBP.evalAddSmall fbpl
    , bench "initial (no cps)" $ 
        nf InitialNoCPS.evalAddSmall inl
    , bench "initial (compose)" $ 
        nf InitialCompose.evalAddSmall icl
    , bench "initial (fuse)" $ 
        nf InitialFuse.evalAddSmall ifl
    , bench "initial-bp" $ 
        nf InitialBP.evalAddSmall ibpl
    ]
  , bgroup "add and mul - small" [
      bench "vanilla" $ 
        nf Vanilla.evalAddMulSmall vl
    , bench "final (int)" $ 
        nf FinalInt.evalAddMulSmall fil
    , bench "final (eval)" $ 
        nf FinalEval.evalAddMulSmall fel
    , bench "final-gadt" $ 
        nf FinalGADT.evalAddMulSmall fgl
    , bench "final-bp" $ 
        nf FinalBP.evalAddMulSmall fbpl
    , bench "initial (no cps)" $ 
        nf InitialNoCPS.evalAddMulSmall inl
    , bench "initial (compose)" $ 
        nf InitialCompose.evalAddMulSmall icl
    , bench "initial (fuse)" $ 
        nf InitialFuse.evalAddMulSmall ifl
    , bench "initial-bp" $ 
        nf InitialBP.evalAddMulSmall ibpl
    ]
  , bgroup "add - big" [
      bench "vanilla" $ 
        nf Vanilla.evalAddBig vl
    , bench "final (int)" $ 
        nf FinalInt.evalAddBig fil
    , bench "final (eval)" $ 
        nf FinalEval.evalAddBig fel
    , bench "final-gadt" $ 
        nf FinalGADT.evalAddBig fgl
    , bench "final-bp" $ 
        nf FinalBP.evalAddBig fbpl
    , bench "initial (no cps)" $ 
        nf InitialNoCPS.evalAddBig inl
    , bench "initial (compose)" $ 
        nf InitialCompose.evalAddBig icl
    , bench "initial (fuse)" $ 
        nf InitialFuse.evalAddBig ifl
    , bench "initial-bp" $ 
        nf InitialBP.evalAddBig ibpl
    ]
  , bgroup "add and mul - big" [
      bench "vanilla" $ 
        nf Vanilla.evalAddMulBig vl
    , bench "final (int)" $ 
        nf FinalInt.evalAddMulBig fil
    , bench "final (eval)" $ 
        nf FinalEval.evalAddMulBig fel
    , bench "final-gadt" $ 
        nf FinalGADT.evalAddMulBig fgl
    , bench "final-bp" $ 
        nf FinalBP.evalAddMulBig fbpl
    , bench "initial (no cps)" $ 
        nf InitialNoCPS.evalAddMulBig inl
    , bench "initial (compose)" $ 
        nf InitialCompose.evalAddMulBig icl
    , bench "initial (fuse)" $ 
        nf InitialFuse.evalAddMulBig ifl
    , bench "initial-bp" $ 
        nf InitialBP.evalAddMulBig ibpl
    ]
  ]
