{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
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

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as T

-- | Render a html document to a String.
{-# INLINE renderString #-}
renderString :: Document a => a -> String
renderString = T.unpack . renderText

-- | Render a html document to a lazy Text.
{-# INLINE renderText #-}
renderText :: Document a => a -> T.Text
renderText = T.decodeUtf8 . renderByteString

-- | Render a html document to a lazy ByteString.
{-# INLINE renderByteString #-}
renderByteString :: Document a => a -> B.ByteString
renderByteString = BE.toLazyByteStringWith
  ( BE.untrimmedStrategy
    1024
    BE.smallChunkSize
  ) B.empty . renderBuilder

-- | Show instances to faciliate ghci development.
instance Document ((a :@: b) c) => Show ((a :@: b) c) where show = renderString
instance Document (a := b)      => Show (a := b)      where show = renderString
instance Document (a # b)       => Show (a # b)       where show = renderString
