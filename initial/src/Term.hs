{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Term (
    Term(..)
  ) where

import Control.Lens

newtype Term f a = Term { unTerm :: f (Term f) a }
--  deriving (Eq, Ord, Show)

makeWrapped ''Term
