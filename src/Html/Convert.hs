{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Html.Convert where

import Data.Proxy
import Data.String
import GHC.TypeLits
import Html.Type

import qualified Data.Text.Lazy                   as T
import qualified Data.Text.Lazy.Encoding          as T
import qualified Data.Text.Lazy.Builder           as TB
import qualified Data.Text.Lazy.Builder.Int       as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.ByteString.Lazy.Char8       as B

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
  conv :: Convert a => a -> Converted b

instance Conv String where
  {-# INLINE conv #-}
  conv = convertString
instance Conv T.Text where
  {-# INLINE conv #-}
  conv = convertText
instance Conv B.ByteString where
  {-# INLINE conv #-}
  conv = convertByteString

{-| Convert a type efficienctly to different string like types.  Add
  instances if you want use custom types in your document.

@
{\-\# LANGUAGE RecordWildCards \#-\}

module Main where

import Html

data Person
  = Person
  { name :: String
  , age :: Int
  , vegetarian :: Bool
  }

-- | This is not efficient, but understandable.
-- The call to convertText is needed for escaping.
-- This is enforced by a newtype. Wrap it in Raw if you don't want to escape.
instance Convert Person where
  convertText (Person{..})
    = convertText
    $  name
    ++ " is "
    ++ show age
    ++ " years old and likes "
    ++ if vegetarian then "oranges." else "meat."

john :: Person
john = Person {name = "John", age = 52, vegetarian = True}

main :: IO ()
main = print (div_ john)
@
-}
class Convert a where

  {-# MINIMAL convertText #-}

  convertString :: a -> Converted String
  {-# INLINE convertString #-}
  convertString = Converted . T.unpack . unConv . convertText

  convertText :: a -> Converted T.Text

  convertByteString :: a -> Converted B.ByteString
  {-# INLINE convertByteString #-}
  convertByteString = Converted . T.encodeUtf8 . unConv . convertText

instance Convert (Raw String) where
  {-# INLINE convertText #-}
  convertText (Raw x) = Converted (T.pack x)
instance Convert (Raw T.Text) where
  {-# INLINE convertText #-}
  convertText (Raw x) = Converted x
instance Convert (Raw B.ByteString) where
  {-# INLINE convertText #-}
  convertText (Raw x) = Converted (T.decodeUtf8 x)
  {-# INLINE convertByteString #-}
  convertByteString (Raw x) = Converted x
instance Convert String where
  {-# INLINE convertString #-}
  convertString = Converted . escapeString
  {-# INLINE convertText #-}
  convertText = Converted . escapeText . T.pack
instance Convert T.Text where
  {-# INLINE convertText #-}
  convertText = Converted . escapeText
instance Convert Int where
  {-# INLINE convertString #-}
  convertString = Converted . show
  {-# INLINE convertText #-}
  convertText = Converted . TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . show
instance Convert Integer where
  {-# INLINE convertString #-}
  convertString = Converted . show
  {-# INLINE convertText #-}
  convertText = Converted . TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . show
instance Convert Float where
  {-# INLINE convertString #-}
  convertString = Converted . show
  {-# INLINE convertText #-}
  convertText = Converted . TB.toLazyText . TB.realFloat
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . show
instance Convert Double where
  convertString = Converted . show
  {-# INLINE convertText #-}
  convertText = Converted . TB.toLazyText . TB.realFloat
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . show
instance Convert Word where
  {-# INLINE convertString #-}
  convertString = Converted . show
  {-# INLINE convertText #-}
  convertText = Converted . TB.toLazyText . TB.decimal
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . show
instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convertString #-}
  convertString = Converted . symbolVal
  {-# INLINE convertText #-}
  convertText = Converted . T.pack . symbolVal
  {-# INLINE convertByteString #-}
  convertByteString = Converted . B.pack . symbolVal
