{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Term (
    Term(..)
  ) where

import Control.Lens

import GHC.Generics

newtype Term f = Term { unTerm :: f (Term f) }
  deriving Generic
--  deriving (Eq, Ord, Show)

makeWrapped ''Term

