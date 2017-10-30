{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeOperators        #-}

module Html
  ( renderString
  , renderText
  , renderByteString
  , renderBuilder
  , Document
  , module Html.Type
  , module Html.Convert
  , module Html.Element
  ) where

import Html.Reify

import Html.Convert

import Html.Element

import Html.Type

-- | Orphan show instances to faciliate ghci development.
instance                     Document ((a :@: b) c) => Show ((a :@: b) c) where show = renderString
instance {-# OVERLAPPING #-} Document ((a :@: b) c) => Show [(a :@: b) c] where show = renderString
instance                     Document (a # b)       => Show (a # b)       where show = renderString
instance {-# OVERLAPPING #-} Document (a # b)       => Show [a # b]       where show = renderString
