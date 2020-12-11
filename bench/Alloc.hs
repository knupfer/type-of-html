{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

-- | Note that the allocation numbers are only reproducible on linux using the nix shell.

module Main where

import Html

import qualified Small             as S
import qualified Medium            as M
import qualified ExampleTypeOfHtml as EX

import Weigh
import Data.Proxy

main :: IO ()
main = mainWith $ do

  f "()"                   id ()                                 [    144 ,   -16 ,      16 ,     0 ,      0 ,      0 ]
  f "Int"                  id (123456789 :: Int)                 [    192 ,     0 ,       0 ,     0 ,      0 ,      0 ]
  f "Word"                 id (123456789 :: Word)                [    192 ,     0 ,       0 ,     0 ,      0 ,      0 ]
  f "Char"                 id 'a'                                [    192 ,     0 ,       0 ,     0 ,      0 ,     16 ]
  f "Integer"              id (123456789 :: Integer)             [    248 ,     0 ,       0 ,     0 ,      0 ,      0 ]
  f "Proxy"                id (Proxy :: Proxy "a")               [    280 ,   -16 ,      16 ,     0 ,      0 ,      0 ]
  f "oneElement Proxy"     S.oneElement (Proxy :: Proxy "b")     [    280 ,   -16 ,      16 ,     0 ,      0 ,      0 ]
  f "oneElement ()"        S.oneElement ()                       [    280 ,   -16 ,      16 ,     0 ,      0 ,     24 ]
  f "oneAttribute ()"      (ClassA :=) ()                        [    280 ,   -16 ,      16 ,     0 ,      0 ,     24 ]
  f "oneAttribute Proxy"   (ClassA :=) (Proxy :: Proxy "c")      [    280 ,   -16 ,      16 ,     0 ,      0 ,      0 ]
  f "listElement"          S.listElement ()                      [    608 ,     0 ,       0 ,     0 ,      0 ,     -8 ]
  f "Double"               id (123456789 :: Double)              [    360 ,     0 ,       0 ,     0 ,    -32 ,     32 ]
  f "oneElement"           S.oneElement ""                       [    368 ,     0 ,       0 ,     0 ,      0 ,     64 ]
  f "nestedElement"        S.nestedElement ""                    [    368 ,     0 ,       0 ,     0 ,      0 ,      0 ]
  f "listOfAttributes"     (\x -> [ClassA := x, ClassA := x]) () [    744 ,     0 ,       0 ,     0 ,      0 ,      8 ]
  f "Float"                id (123456789 :: Float)               [    400 ,     0 ,       0 ,     0 ,    -72 ,     72 ]
  f "oneAttribute"         (ClassA :=) ""                        [    480 ,     0 ,       0 ,     0 ,      0 ,     64 ]
  f "parallelElement"      S.parallelElement ""                  [    520 ,     0 ,       0 ,   -16 ,      0 ,     40 ]
  f "parallelAttribute"    (\x -> ClassA := x # IdA := x) ""     [    672 ,     0 ,       0 ,   -16 ,      0 ,    104 ]
  f "elementWithAttribute" (\x -> Div :@ (ClassA := x) :> x) ""  [    656 ,     0 ,       0 ,     0 ,      0 ,     64 ]
  f "listOfListOf"         (\x -> Div :> [I :> [Span :> x]]) ()  [   1200 ,     0 ,      64 ,     0 ,      0 ,    -32 ]
  f "helloWorld"           M.helloWorld ()                       [   1232 ,   -16 ,      16 ,     0 ,      0 ,    216 ]
  f "page"                 M.page ()                             [   1416 ,   -16 ,    -144 ,     0 ,      0 ,    200 ]
  f "table"                M.table (2,2)                         [   2616 ,     0 ,      64 ,   -96 ,     64 ,   -120 ]
  f "AttrShort"            M.attrShort ()                        [   2680 ,   -16 ,     856 ,     0 ,      0 ,    552 ]
  f "pageA"                M.pageA ()                            [   2504 ,  -176 ,      64 ,     0 ,      0 ,   1160 ]
  f "AttrLong"             M.attrLong ()                         [   2680 ,   -16 ,     856 ,     0 ,      0 ,    736 ]
  f "Big table"            M.table (50,10)                       [ 120072 ,     0 ,    1600 , -8096 ,   7952 , -15288 ]
  f "hackage upload"       EX.hackageUpload ()                   [ 698784 ,   488 , -204792 , -1408 , -47120 , -64368 ]

  where versions =                                               [    802 ,   804 ,     806 ,   808 ,    810 ,    900 ]

        ghc = fromInteger . sum . take (length $ takeWhile (<= (__GLASGOW_HASKELL__ :: Int)) versions)

        f s g x n = validateFunc s (renderByteString . g) x (allocs $ ghc n)

        allocs n w
          | n' > n = Just $ "More" ++ answer (n' - n)
          | n' < n = Just $ "Less" ++ answer (n - n')
          | otherwise = Nothing
          where n' = weightAllocatedBytes w
                answer x = " allocated bytes than expected: " ++ show x



