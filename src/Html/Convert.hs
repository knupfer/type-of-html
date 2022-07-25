{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}

module Html.Convert
  ( Converted(..)
  , Convert(..)
  ) where

import Html.Type.Internal

import Data.Word
import Data.Proxy
import Data.String
import Data.Char (ord)
import Data.Double.Conversion.ByteString
import Numeric.Natural
import GHC.Exts (build)
import GHC.TypeLits
import GHC.CString

import qualified Data.Semigroup                   as S
import qualified Data.Monoid                      as M
import qualified Data.ByteString.Builder          as B
import qualified Data.ByteString.Builder.Prim     as BP
import qualified Data.ByteString.Builder.Internal as U
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TL

newtype Converted = Converted {unConv :: B.Builder} deriving (M.Monoid, S.Semigroup)

instance IsString Converted where fromString = convert

{-| Convert a type efficienctly to a renderable representation.  Add
  instances if you want use custom types in your document.

@
{\-\# LANGUAGE OverloadedStrings \#-\}
{\-\# LANGUAGE RecordWildCards   \#-\}

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
john = Person {name = \"John\", age = 52, vegetarian = True}

main :: IO ()
main = print (div_ john)
@
-}
class Convert a where
  convert :: a -> Converted

instance KnownSymbol a => Convert (Proxy a) where {-# INLINE convert #-}; convert = Converted . U.byteStringCopy . fromString . symbolVal

instance Convert ()              where {-# INLINE convert #-}; convert = const mempty
instance Convert Converted       where {-# INLINE convert #-}; convert = id
instance Convert (Raw Char)      where {-# INLINE convert #-}; convert = Converted . B.charUtf8 . fromRaw
instance Convert (Raw String)    where {-# INLINE convert #-}; convert = Converted . fromString . fromRaw
instance Convert (Raw T.Text)    where {-# INLINE convert #-}; convert = Converted . T.encodeUtf8Builder . fromRaw
instance Convert (Raw TL.Text)   where {-# INLINE convert #-}; convert = Converted . TL.encodeUtf8Builder . fromRaw
instance Convert (Raw B.Builder) where {-# INLINE convert #-}; convert = Converted . fromRaw
instance Convert Char            where {-# INLINE convert #-}; convert = convert . T.singleton
instance Convert String          where {-# INLINE convert #-}; convert = stringConv
instance Convert T.Text          where {-# INLINE convert #-}; convert = Converted . T.encodeUtf8BuilderEscaped escape
instance Convert TL.Text         where {-# INLINE convert #-}; convert = Converted . TL.encodeUtf8BuilderEscaped escape
instance Convert Int             where {-# INLINE convert #-}; convert = Converted . B.intDec
instance Convert Integer         where {-# INLINE convert #-}; convert = Converted . B.integerDec
instance Convert Natural         where {-# INLINE convert #-}; convert = Converted . B.integerDec . fromIntegral
instance Convert Float           where {-# INLINE convert #-}; convert = Converted . U.byteStringCopy . toShortest . realToFrac
instance Convert Double          where {-# INLINE convert #-}; convert = Converted . U.byteStringCopy . toShortest
instance Convert Word            where {-# INLINE convert #-}; convert = Converted . B.wordDec
instance Convert a => Convert (Maybe a) where {-# INLINE convert #-}; convert = maybe mempty convert
instance (Convert a, Convert b) => Convert (Either a b) where {-# INLINE convert #-}; convert = either convert convert

{-# INLINE [0] stringConv #-}
stringConv :: String -> Converted
stringConv = convert . T.pack

escape :: BP.BoundedPrim Word8
escape
  = BP.condB (>  c2w '>' ) (BP.liftFixedToBounded BP.word8)
  . BP.condB (== c2w '<' ) (fixed4 (c2w '&',(c2w 'l',(c2w 't',c2w ';'))))
  . BP.condB (== c2w '>' ) (fixed4 (c2w '&',(c2w 'g',(c2w 't',c2w ';'))))
  . BP.condB (== c2w '&' ) (fixed5 (c2w '&',(c2w 'a',(c2w 'm',(c2w 'p',c2w ';')))))
  . BP.condB (== c2w '"' ) (fixed5 (c2w '&',(c2w '#',(c2w '3',(c2w '4',c2w ';')))))
  . BP.condB (== c2w '\'') (fixed5 (c2w '&',(c2w '#',(c2w '3',(c2w '9',c2w ';')))))
  $ BP.liftFixedToBounded BP.word8
  where
    c2w = fromIntegral . ord

    {-# INLINE fixed4 #-}
    fixed4 x = BP.liftFixedToBounded $ const x BP.>$<
      BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8

    {-# INLINE fixed5 #-}
    fixed5 x = BP.liftFixedToBounded $ const x BP.>$<
      BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8

{-# RULES "CONVERTED literal" forall a.
    stringConv (unpackCString# a) = Converted (BP.primMapByteStringBounded escape (fromString (unpackCString# a))) #-}

{-# RULES "CONVERTED foldr literal" forall a.
    stringConv (build (unpackFoldrCString# a)) = Converted (BP.primMapByteStringBounded escape (fromString (unpackCString# a))) #-}
