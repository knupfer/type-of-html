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

import qualified Small          as S
import qualified Medium         as M
import qualified Big            as B

import Weigh
import Data.Proxy

main :: IO ()
main = mainWith $ do

  f "()"                   id ()                                [   144 ,  -16 ,   16 ,     0 ,    0 ]
  f "Int"                  id (123456789 :: Int)                [   192 ,    0 ,    0 ,     0 ,    0 ]
  f "Word"                 id (123456789 :: Word)               [   192 ,    0 ,    0 ,     0 ,    0 ]
  f "Char"                 id 'a'                               [   192 ,    0 ,    0 ,     0 ,    0 ]
  f "Integer"              id (123456789 :: Integer)            [   248 ,    0 ,    0 ,     0 ,    0 ]
  f "Proxy"                id (Proxy :: Proxy "a")              [   280 ,  -16 ,   16 ,     0 ,    0 ]
  f "oneElement Proxy"     S.oneElement (Proxy :: Proxy "b")    [   280 ,  -16 ,   16 ,     0 ,    0 ]
  f "oneElement ()"        S.oneElement ()                      [   280 ,  -16 ,   16 ,     0 ,    0 ]
  f "oneAttribute ()"      (ClassA :=) ()                          [   280 ,  -16 ,   16 ,     0 ,    0 ]
  f "oneAttribute Proxy"   (ClassA :=) (Proxy :: Proxy "c")        [   280 ,  -16 ,   16 ,     0 ,    0 ]
  f "listElement"          S.listElement ()                     [   608 ,    0 ,    0 ,     0 ,    0 ]
  f "Double"               id (123456789 :: Double)             [   360 ,    0 ,    0 ,     0 ,  -32 ]
  f "oneElement"           S.oneElement ""                      [   368 ,    0 ,    0 ,     0 ,    0 ]
  f "nestedElement"        S.nestedElement ""                   [   368 ,    0 ,    0 ,     0 ,    0 ]
  f "listOfAttributes"     (\x -> [ClassA := x, ClassA := x]) ()  [   712 ,    0 ,    0 ,     0 ,    0 ]
  f "Float"                id (123456789 :: Float)              [   400 ,    0 ,    0 ,     0 ,  -72 ]
  f "oneAttribute"         (ClassA :=) ""                          [   520 ,    0 ,    0 ,     0 ,    0 ]
  f "parallelElement"      S.parallelElement ""                 [   520 ,    0 ,    0 ,   -16 ,    0 ]
  f "parallelAttribute"    (\x -> ClassA := x # IdA := x) ""      [   736 ,    0 ,    0 ,     0 ,    0 ]
  f "elementWithAttribute" (\x -> Div :@ (ClassA := x) :> x) ""      [   696 ,    0 ,    0 ,     0 ,    0 ]
  f "listOfListOf"         (\x -> Div :> [I :> [Span :> x]]) ()       [  1200 ,    0 ,   64 ,     0 ,    0 ]
  f "helloWorld"           M.helloWorld ()                      [  1232 ,  -16 ,   16 ,     0 ,    0 ]
  f "page"                 M.page ()                            [  1416 ,  -16 , -144 ,     0 ,    0 ]
  f "table"                M.table (2,2)                        [  2680 ,    0 ,    0 ,   -96 ,   64 ]
  f "AttrShort"            M.attrShort ()                       [  2704 ,  -16 ,  832 ,     0 ,    0 ]
  f "pageA"                M.pageA ()                           [  4320 ,  136 , -264 ,  -288 ,  184 ]
  f "AttrLong"             M.attrLong ()                        [  2704 ,  -16 ,  832 ,     0 ,    0 ]
  f "Big table"            M.table (15,15)                      [ 54080 ,    0 ,    0 , -3736 , 3576 ]
  f "Big page"             B.page ()                            [ 27920 ,  -96 ,    0 , -1344 ,  480 ]

  where versions =                                              [   802 ,  804 ,  806 ,   808 ,  810 ]

        ghc = fromInteger . sum . take (length $ takeWhile (<= (__GLASGOW_HASKELL__ :: Int)) versions)

        f s g x n = validateFunc s (renderByteString . g) x (allocs $ ghc n)

        allocs n w
          | n' > n = Just $ "More" ++ answer (n' - n)
          | n' < n = Just $ "Less" ++ answer (n - n')
          | otherwise = Nothing
          where n' = weightAllocatedBytes w
                answer x = " allocated bytes than expected: " ++ show x



