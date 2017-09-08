{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}

module Html.Convert where

import Data.Proxy
import Data.String
import GHC.TypeLits
import Html.Type

import Data.Char (ord)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Internal as U

import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

{-# INLINE escape #-}
escape :: T.Text -> B.Builder
escape = T.encodeUtf8BuilderEscaped $
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
  convert :: a -> Converted B.Builder

instance Convert (Raw String) where
  {-# INLINE convert #-}
  convert (Raw x) = Converted (fromString x)
instance Convert (Raw T.Text) where
  {-# INLINE convert #-}
  convert (Raw x) = Converted (T.encodeUtf8Builder x)
instance Convert String where
  {-# INLINE convert #-}
  convert = Converted . escape . fromString
instance Convert T.Text where
  {-# INLINE convert #-}
  convert = Converted . escape
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
