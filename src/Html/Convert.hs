{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Html.Convert where

import Data.Proxy
import Data.String
import GHC.TypeLits

import qualified Data.Text.Lazy                   as T
import qualified Data.Text.Lazy.Encoding          as T
import qualified Data.Text.Lazy.Builder           as TB
import qualified Data.Text.Lazy.Builder.Int       as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.ByteString.Lazy.Char8       as B

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw a

{-# INLINE escapeString #-}
escapeString :: String -> String
escapeString = concatMap $ escape pure

{-# INLINE escapeText #-}
escapeText :: T.Text -> T.Text
escapeText = T.concatMap $ escape T.singleton

{-# INLINE escapeByteString #-}
escapeByteString :: B.ByteString -> B.ByteString
escapeByteString = B.concatMap $ escape B.singleton

{-# INLINE escape #-}
escape :: IsString a => (Char -> a) -> Char -> a
escape f = \case
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '&'  -> "&amp;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  x    -> f x

class Conv b where
  conv :: Convert a => a -> b

instance Conv String where
  {-# INLINE conv #-}
  conv = convertString
instance Conv T.Text where
  {-# INLINE conv #-}
  conv = convertText
instance Conv B.ByteString where
  {-# INLINE conv #-}
  conv = convertByteString

-- | Convert a type efficienctly to different string like types.
class Convert a where

  {-# MINIMAL convertText #-}

  convertString :: a -> String
  {-# INLINE convertString #-}
  convertString = T.unpack . convertText

  convertText :: a -> T.Text

  convertByteString :: a -> B.ByteString
  {-# INLINE convertByteString #-}
  convertByteString = T.encodeUtf8 . convertText

instance Convert (Raw String) where
  {-# INLINE convertText #-}
  convertText (Raw x) = T.pack x
instance Convert (Raw T.Text) where
  {-# INLINE convertText #-}
  convertText (Raw x) = x
instance Convert (Raw B.ByteString) where
  {-# INLINE convertText #-}
  convertText (Raw x) = T.decodeUtf8 x
  {-# INLINE convertByteString #-}
  convertByteString (Raw x) = x
instance Convert String where
  {-# INLINE convertString #-}
  convertString = escapeString
  {-# INLINE convertText #-}
  convertText = escapeText . T.pack
instance Convert T.Text where
  {-# INLINE convertText #-}
  convertText = escapeText
instance Convert Int where
  {-# INLINE convertString #-}
  convertString = show
  {-# INLINE convertText #-}
  convertText = TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . show
instance Convert Integer where
  {-# INLINE convertString #-}
  convertString = show
  {-# INLINE convertText #-}
  convertText = TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . show
instance Convert Float where
  {-# INLINE convertString #-}
  convertString = show
  {-# INLINE convertText #-}
  convertText = TB.toLazyText . TB.realFloat
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . show
instance Convert Double where
  convertString = show
  {-# INLINE convertText #-}
  convertText = TB.toLazyText . TB.realFloat
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . show
instance Convert Word where
  {-# INLINE convertString #-}
  convertString = show
  {-# INLINE convertText #-}
  convertText = TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . show
instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convertString #-}
  convertString = symbolVal
  {-# INLINE convertText #-}
  convertText = T.pack . symbolVal
  {-# INLINE convertByteString #-}
  convertByteString = B.pack . symbolVal
