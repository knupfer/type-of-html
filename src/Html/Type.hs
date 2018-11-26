{-# LANGUAGE ExplicitNamespaces #-}

module Html.Type
  ( type (>)
  , type (:@:)(..)
  , type (#)(..)
  , (#)
  , type (?>)
  , type (<?>)
  , type (:=)(..)
  , Raw(..)
  , Attribute(..)
  , Element(..)
  , CompactHTML
  , Put(..)
  , V(..)
  , Retrievable
  , Retrieve
  , Variables
  , Document
  , Compactable
  ) where

import Html.Type.Internal
import Html.Reify
