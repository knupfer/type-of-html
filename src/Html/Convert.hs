{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

module Html.Convert where

import Data.Word
import Data.Proxy
import Data.String
import GHC.TypeLits
import Html.Type

import Data.Char (ord)

import qualified Data.Monoid as M
import qualified Data.Semigroup as S

import qualified Data.ByteString.Internal as U
import qualified Data.ByteString.Unsafe   as U
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Internal as U

import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL

{-# INLINE unsafe #-}
unsafe i addr = U.accursedUnutterablePerformIO (U.unsafePackAddressLen i addr)

{-# INLINE escape #-}
escape :: BP.BoundedPrim Word8
escape =
    BP.condB (>  c2w '>' ) (BP.liftFixedToBounded BP.word8) $
    BP.condB (== c2w '<' ) (fixed4 (c2w '&',(c2w 'l',(c2w 't',c2w ';')))) $        -- &lt;
    BP.condB (== c2w '>' ) (fixed4 (c2w '&',(c2w 'g',(c2w 't',c2w ';')))) $        -- &gt;
    BP.condB (== c2w '&' ) (fixed5 (c2w '&',(c2w 'a',(c2w 'm',(c2w 'p',c2w ';'))))) $  -- &amp;
    BP.condB (== c2w '"' ) (fixed5 (c2w '&',(c2w '#',(c2w '3',(c2w '4',c2w ';'))))) $  -- &#34;
    BP.condB (== c2w '\'') (fixed5 (c2w '&',(c2w '#',(c2w '3',(c2w '9',c2w ';'))))) $  -- &#39;
    BP.liftFixedToBounded BP.word8         -- fallback for Chars smaller than '>'
  where

    {-# INLINE c2w #-}
    c2w = fromIntegral . ord

    {-# INLINE fixed4 #-}
    fixed4 x = BP.liftFixedToBounded $ const x BP.>$<
      BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8

    {-# INLINE fixed5 #-}
    fixed5 x = BP.liftFixedToBounded $ const x BP.>$<
      BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8

{-| Convert a type efficienctly to different string like types.  Add
  instances if you want use custom types in your document.

@
{\-\# LANGUAGE RecordWildCards \#-\}
{\-\# LANGUAGE OverloadedStrings \#-\}

module Main where

import Html

import Data.Text (Text)
import Data.Monoid

data Person
  = Person
  { name :: Text
  , age :: Int
  , vegetarian :: Bool
  }

-- | This is already very efficient.
-- Wrap the Strings in Raw if you don't want to escape them.

instance Convert Person where
  convert (Person{..})
    =  convert name
    <> " is "
    <> convert age
    <> " years old and likes "
    <> if vegetarian then "oranges." else "meat."

john :: Person
john = Person {name = "John", age = 52, vegetarian = True}

main :: IO ()
main = print (div_ john)
@
-}

newtype Converted = Converted {unConv :: B.Builder} deriving (M.Monoid,S.Semigroup)

instance IsString Converted where
  fromString = convert

class Convert a where
  convert :: a -> Converted

instance Convert (Raw String) where
  {-# INLINE convert #-}
  convert (Raw x) = Converted (fromString x)
instance Convert (Raw T.Text) where
  {-# INLINE convert #-}
  convert (Raw x) = Converted (T.encodeUtf8Builder x)
instance Convert (Raw TL.Text) where
  {-# INLINE convert #-}
  convert (Raw x) = Converted (TL.encodeUtf8Builder x)
instance Convert String where
  {-# INLINE convert #-}
  convert = convert . T.pack
instance Convert T.Text where
  {-# INLINE convert #-}
  convert = Converted . T.encodeUtf8BuilderEscaped escape
instance Convert TL.Text where
  {-# INLINE convert #-}
  convert = Converted . TL.encodeUtf8BuilderEscaped escape
instance Convert Int where
  {-# INLINE convert #-}
  convert = Converted . B.intDec
instance Convert Integer where
  {-# INLINE convert #-}
  convert = Converted . B.integerDec
instance Convert Float where
  {-# INLINE convert #-}
  convert = Converted . B.floatDec
instance Convert Double where
  {-# INLINE convert #-}
  convert = Converted . B.doubleDec
instance Convert Word where
  {-# INLINE convert #-}
  convert = Converted . B.wordDec
instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convert #-}
  convert = Converted . U.byteStringCopy . fromString . symbolVal
