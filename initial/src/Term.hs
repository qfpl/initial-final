{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Term (
    Term(..)
  ) where

import Control.Lens

import Control.DeepSeq (NFData)
import GHC.Generics

newtype Term f a = Term { unTerm :: f (Term f) a }
  deriving Generic
--  deriving (Eq, Ord, Show)

makeWrapped ''Term

