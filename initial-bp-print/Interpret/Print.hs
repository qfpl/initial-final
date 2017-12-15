module Interpret.Print (
    PrintRule(..)
  , mkPrint
  ) where

import Data.Foldable (asum)

data PrintRule tm =
  PrintRule ((tm -> Maybe String) -> tm -> Maybe String)

fixPrint :: (tm -> Maybe String) -> tm -> PrintRule tm -> Maybe String
fixPrint pr tm (PrintRule f)  = f pr tm

mkPrint :: [PrintRule tm] -> tm -> Maybe String
mkPrint rules =
  let
    pr tm = asum . fmap (fixPrint pr tm) $ rules
  in
    pr
