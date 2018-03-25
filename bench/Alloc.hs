{-# LANGUAGE DataKinds #-}

module Main where

import Html
import qualified Html.Attribute as A
import qualified Small          as S
import qualified Medium         as M
import qualified Big            as B

import Weigh
import Control.DeepSeq
import Data.Proxy
import Data.Int

allocs :: Int64 -> Weight -> Maybe String
allocs n w
  | n' > n = Just $ "More" ++ answer
  | n' < n = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = weightAllocatedBytes w
        answer = " allocated bytes than " ++ commas n ++ ": " ++ commas n'

f :: NFData b => String -> Int64 -> (a -> b) -> a -> Weigh ()
f s n g x = validateFunc s g x (allocs n)

main :: IO ()
main = mainWith $ do

  -- nixpkgs: 1b27260
  -- ghc: 8.4.1

  f "()"                             144 renderByteString ()
  f "Int"                            216 renderByteString (123456789 :: Int)
  f "Word"                           216 renderByteString (123456789 :: Word)
  f "Char"                           232 renderByteString 'a'
  f "Integer"                        248 renderByteString (123456789 :: Integer)
  f "Proxy"                          280 renderByteString (Proxy :: Proxy "a")
  f "oneElement Proxy"               280 (renderByteString . S.oneElement) (Proxy :: Proxy "b")
  f "oneElement ()"                  280 (renderByteString . S.oneElement) ()
  f "oneAttribute ()"                280 (renderByteString . A.class_) ()
  f "oneAttribute Proxy"             280 (renderByteString . A.class_) (Proxy :: Proxy "c")
  f "listElement"                    376 (renderByteString . S.listElement) ()
  f "Double"                         360 renderByteString (123456789 :: Double)
  f "oneElement"                     368 (renderByteString . S.oneElement) ""
  f "nestedElement"                  368 (renderByteString . S.nestedElement) ""
  f "listOfAttributes"               352 (\x -> renderByteString [A.class_ x, A.class_ x]) ()
  f "Float"                          400 renderByteString (123456789 :: Float)
  f "oneAttribute"                   408 (renderByteString . A.class_) ""
  f "parallelElement"                520 (renderByteString . S.parallelElement) ""
  f "parallelAttribute"              584 (\x -> renderByteString $ A.class_ x # A.id_ x) ""
  f "elementWithAttribute"           584 (\x -> renderByteString $ div_A (A.class_ x) x) ""
  f "listOfListOf"                   632 (\x -> renderByteString $ div_ [i_ [span_ x]]) ()
  f "helloWorld"                    1232 (renderByteString . M.helloWorld) ()
  f "page"                          1512 (renderByteString . M.page) ()
  f "table"                         1664 (renderByteString . M.table) (2,2)
  f "AttrShort"                     2608 (renderByteString . M.attrShort) ()
  f "pageA"                         2960 (renderByteString . M.pageA) ()
  f "AttrLong"                      3160 (renderByteString . M.attrLong) ()
  f "Big table"                    19968 (renderByteString . M.table) (15,15)
  f "Big page"                     25064 (renderByteString . B.page) ()
