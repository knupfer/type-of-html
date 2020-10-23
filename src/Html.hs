{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}

module Html
  ( module Html.Type
  , module Html.Convert
  , module Html.Element
  , renderString
  , renderText
  , renderByteString
  , renderBuilder
  , compactHTML
  , renderCompactString
  , renderCompactText
  , renderCompactByteString
  , renderCompactBuilder
  , Put(..)
  , V(..)
  ) where

import Html.Reify
import Html.Convert
import Html.Element
import Html.Type
import Html.Type.Internal

import Control.Arrow
import GHC.Exts
import Data.Either
import Data.List
import Data.ByteString.Builder
import Data.Maybe

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as T

-- | Render a html document to a Builder.
renderBuilder :: Document a => a -> Builder
renderBuilder = unConv . (render @ 'False . (T :: a -> T (ToList a) a))

-- | Render a html document to a String.
renderString :: Document a => a -> String
renderString = T.unpack . renderText

-- | Render a html document to a lazy Text.
renderText :: Document a => a -> T.Text
renderText = T.decodeUtf8 . renderByteString

-- | Render a html document to a lazy ByteString.
renderByteString :: Document a => a -> BL.ByteString
renderByteString = BE.toLazyByteStringWith
  ( BE.untrimmedStrategy
    1024
    BE.smallChunkSize
  ) BL.empty . renderBuilder

renderCompactHTML :: Retrievable a => (Builder -> f) -> CompactHTML a -> Retrieve f a
renderCompactHTML = retrieve mempty

-- | Render a compacted html document to a Builder.
renderCompactBuilder :: Retrievable a => CompactHTML a -> Retrieve Builder a
renderCompactBuilder = renderCompactHTML id

-- | Render a compacted html document to a lazy ByteString.
renderCompactByteString :: Retrievable a => CompactHTML a -> Retrieve BL.ByteString a
renderCompactByteString = renderCompactHTML toLazyBS

-- | Render a compacted html document to a lazy Text.
renderCompactText :: Retrievable a => CompactHTML a -> Retrieve T.Text a
renderCompactText = renderCompactHTML (T.decodeUtf8 . toLazyBS)

-- | Render a compacted html document to a String.
renderCompactString :: Retrievable a => CompactHTML a -> Retrieve String a
renderCompactString = renderCompactHTML (T.unpack . T.decodeUtf8 . toLazyBS)

toLazyBS :: Builder -> BL.ByteString
toLazyBS = BE.toLazyByteStringWith (BE.untrimmedStrategy 1024 BE.smallChunkSize) BL.empty

-- | Compact a html document.
compactHTML :: Compactable a => a -> CompactHTML (Variables a)
compactHTML html
  = uncurry MkCompactHTML
  . concatEithers
  . toList
  . render @ 'True
  $ (T :: a -> T (ToList a) a) html
  where
    concatEithers :: [Either Converted String] -> (B.ByteString, [(Int, B.ByteString)])
    concatEithers = (f *** go) . span isLeft
      where go (Right r1:xs) = let (ls,rs) = span isLeft xs
                               in (indexVar html r1, f ls) : go rs
            go _ = []
            f = BL.toStrict . toLazyByteString . unConv . mconcat . lefts
    indexVar :: forall a. Compactable a => a -> String -> Int
    indexVar _ s = fromJust (elemIndex s (showTypeList @ (Reverse (Variables a))))

-- | Show instances to faciliate ghci development.
instance Document ((a :@: b) c) => Show ((a :@: b) c) where show = renderString
instance Document (a := b)      => Show (a := b)      where show = renderString
instance Document (a # b)       => Show (a # b)       where show = renderString
