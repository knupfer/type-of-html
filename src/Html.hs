{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE TypeOperators             #-}

module Html
  ( renderString
  , renderText
  , renderByteString
  , renderBuilder
  , type (>)(..)
  , type (:@:)(..)
  , type (#)(..)
  , (#)
  , type (?>)
  , type (??>)
  , type (:=)(..)
  , Raw(..)
  , Convert(..)
  , Converted
  , Attribute(..)
  , Element(..)
  , module Html.Element
  ) where

import Html.Reify

import Html.Convert

import Html.Element

import Html.Type

-- | Orphan show instances to faciliate ghci development.
instance                     Document (a > b)       => Show (a > b)       where show = renderString
instance {-# OVERLAPPING #-} Document (a > b)       => Show [a > b]       where show = renderString
instance                     Document ((a :@: b) c) => Show ((a :@: b) c) where show = renderString
instance {-# OVERLAPPING #-} Document ((a :@: b) c) => Show [(a :@: b) c] where show = renderString
instance                     Document (a # b)       => Show (a # b)       where show = renderString
instance {-# OVERLAPPING #-} Document (a # b)       => Show [a # b]       where show = renderString
