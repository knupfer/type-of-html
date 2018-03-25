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

  f "()"                            4256 renderByteString ()
  f "Int"                           4328 renderByteString (123456789 :: Int)
  f "Word"                          4328 renderByteString (123456789 :: Word)
  f "Char"                          4344 renderByteString 'a'
  f "Integer"                       4360 renderByteString (123456789 :: Integer)
  f "Proxy"                         4392 renderByteString (Proxy :: Proxy "a")
  f "oneElement Proxy"              4392 (renderByteString . S.oneElement) (Proxy :: Proxy "b")
  f "oneElement ()"                 4392 (renderByteString . S.oneElement) ()
  f "oneAttribute ()"               4392 (renderByteString . A.class_) ()
  f "oneAttribute Proxy"            4392 (renderByteString . A.class_) (Proxy :: Proxy "c")
  f "listElement"                   4488 (renderByteString . S.listElement) ()
  f "Double"                        4472 renderByteString (123456789 :: Double)
  f "oneElement"                    4480 (renderByteString . S.oneElement) ""
  f "nestedElement"                 4480 (renderByteString . S.nestedElement) ""
  f "listOfAttributes"              4464 (\x -> renderByteString [A.class_ x, A.class_ x]) ()
  f "Float"                         4512 renderByteString (123456789 :: Float)
  f "oneAttribute"                  4520 (renderByteString . A.class_) ""
  f "parallelElement"               4632 (renderByteString . S.parallelElement) ""
  f "parallelAttribute"             4696 (\x -> renderByteString $ A.class_ x # A.id_ x) ""
  f "elementWithAttribute"          4696 (\x -> renderByteString $ div_A (A.class_ x) x) ""
  f "listOfListOf"                  4744 (\x -> renderByteString $ div_ [i_ [span_ x]]) ()
  f "helloWorld"                    5344 (renderByteString . M.helloWorld) ()
  f "page"                          5624 (renderByteString . M.page) ()
  f "table"                         5776 (renderByteString . M.table) (2,2)
  f "AttrShort"                     6720 (renderByteString . M.attrShort) ()
  f "pageA"                         7072 (renderByteString . M.pageA) ()
  f "AttrLong"                      7272 (renderByteString . M.attrLong) ()
  f "Big table"                    19712 (renderByteString . M.table) (15,15)
  f "Big page"                     24704 (renderByteString . B.page) ()
