{-# OPTIONS_GHC -freduction-depth=0 #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

{-# LANGUAGE CPP #-}

module Main where

import Html

import qualified Blaze          as BL
import qualified Small          as S
import qualified Medium         as M
import qualified Big            as B

import Criterion.Main
import Data.String
import System.Random
import System.IO.Unsafe
import Data.Proxy
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Text.Lazy   as LT
import qualified Data.Text        as T

#if __GLASGOW_HASKELL__ <= 802
import Data.Semigroup ((<>), Semigroup)
#endif

main :: IO ()
main = defaultMain
  [ small
  , medium
  , big
  , comparison
  , scaling
  ]

small :: Benchmark
small = bgroup "Small"
  [ bench "oneElement"                   $ nf (renderByteString . S.oneElement) ""
  , bench "nestedElement"                $ nf (renderByteString . S.nestedElement) ""
  , bench "parallelElement"              $ nf (renderByteString . S.parallelElement) ""
  , bench "listElement"                  $ nf (renderByteString . S.listElement) ""
  , bench "Int"                          $ nf renderByteString (123456789 :: Int)
  , bench "Integer"                      $ nf renderByteString (123456789 :: Integer)
  , bench "Double"                       $ nf renderByteString (123456789 :: Double)
  , bench "Float"                        $ nf renderByteString (123456789 :: Float)
  , bench "Word"                         $ nf renderByteString (123456789 :: Word)
  , bench "Proxy"                        $ nf renderByteString (Proxy :: Proxy "abc")
  , bench "Char"                         $ nf renderByteString 'a'
  , bench "oneElement Proxy"             $ nf (renderByteString . S.oneElement) (Proxy :: Proxy "abc")
  , bench "()"                           $ nf renderByteString ()
  , bench "oneElement ()"                $ nf (renderByteString . S.oneElement) ()
  , bench "oneAttribute"                 $ nf (renderByteString . (ClassA :=)) ""
  , bench "oneAttribute ()"              $ nf (renderByteString . (ClassA :=))  ()
  , bench "oneAttribute Proxy"           $ nf (renderByteString . (ClassA :=))  (Proxy :: Proxy "abc")
  , bench "parallelAttribute"            $ nf (\x -> renderByteString $ ClassA := x # IdA := x) ""
  , bench "elementWithAttribute"         $ nf (\x -> renderByteString $ Div :@ (ClassA := x) :> x) ""
  , bench "elementWithParallelAttribute" $ nf (\x -> renderByteString $ Div :@ (ClassA := x # IdA := x) :> x) ""
  , bench "listOfAttributes"             $ nf (\x -> renderByteString [ClassA := x, ClassA := x]) ""
  , bench "listOfListOf"                 $ nf (\x -> renderByteString $ Div :> [I :> [Span :> x]]) ""
  ]

medium :: Benchmark
medium = bgroup "Medium"
  [ bench "helloWorld"          $ nf (renderByteString . M.helloWorld) "medium"
  , bench "randomString"        $ nf renderByteString randomString
  , bench "randomStringRaw"     $ nf renderByteString randomStringRaw
  , bench "randomStrictText"    $ nf renderByteString randomStrictText
  , bench "randomStrictTextRaw" $ nf renderByteString randomStrictTextRaw
  , bench "randomLazyText"      $ nf renderByteString randomLazyText
  , bench "randomLazyTextRaw"   $ nf renderByteString randomLazyTextRaw
  , bench "table"               $ nf (renderByteString . M.table) (2,2)
  , bench "page"                $ nf (renderByteString . M.page) "medium"
  , bench "pageA"               $ nf (renderByteString . M.pageA) "medium"
  , bench "attrShort"           $ nf (renderByteString . M.attrShort) "medium"
  , bench "attrLong"            $ nf (renderByteString . M.attrLong) "medium"
  ]

{-# NOINLINE randomString #-}
randomString :: String
randomString = unsafePerformIO $ take 250 . randoms <$> newStdGen

{-# NOINLINE randomStringRaw #-}
randomStringRaw :: Raw String
randomStringRaw = unsafePerformIO $ Raw . take 250 . randoms <$> newStdGen

{-# NOINLINE randomStrictText #-}
randomStrictText :: T.Text
randomStrictText = unsafePerformIO $ T.pack . take 250 . randoms <$> newStdGen

{-# NOINLINE randomStrictTextRaw #-}
randomStrictTextRaw :: Raw T.Text
randomStrictTextRaw = unsafePerformIO $ Raw . T.pack . take 250 . randoms <$> newStdGen

{-# NOINLINE randomLazyText #-}
randomLazyText :: LT.Text
randomLazyText = unsafePerformIO $ LT.pack . take 250 . randoms <$> newStdGen

{-# NOINLINE randomLazyTextRaw #-}
randomLazyTextRaw :: Raw LT.Text
randomLazyTextRaw = unsafePerformIO $ Raw . LT.pack . take 250 . randoms <$> newStdGen

big :: Benchmark
big = bgroup "Big"
  [ bench "table"  $ nf (renderByteString . M.table) (15,15)
  , bench "page"   $ nf (renderByteString . B.page) "big"
  ]

comparison :: Benchmark
comparison = bgroup "Comparison"
  [ bgroup "hello world"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeHelloWorld) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.helloWorld) "TEST"
    , bench "compactHTML"  $ nf (renderCompactByteString (compactHTML $ M.helloWorld (V @ "x"))) (Put "TEST")
    ]
  , bgroup "table"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeTable) (4,4)
    , bench "type-of-html" $ nf (renderByteString . M.table) (4,4)
    ]
  , bgroup "small page"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazePageA) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.pageA) "TEST"
    , bench "compactHTML"  $ nf (renderCompactByteString (compactHTML $ M.pageA (V @ "x"))) (Put "TEST")
    ]
  , bgroup "medium page"
    [ bench "blaze-html"   $ nf (renderHtml . (\x -> BL.blazePageA x <> BL.blazePageA x)) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . (\x -> M.pageA x # M.pageA x)) "TEST"
    , bench "compactHTML"  $ nf (renderCompactByteString (compactHTML $ M.pageA (V @ "x") # M.pageA (V @ "x"))) (Put "TEST")
    ]
  , bgroup "big page"
    [ bench "blaze-html"   $ nf (renderHtml . (\x -> BL.blazePageA x <> BL.blazePageA x <> BL.blazePageA x <> BL.blazePageA x)) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . (\x -> M.pageA x # M.pageA x # M.pageA x # M.pageA x)) "TEST"
    , bench "compactHTML"  $ nf (renderCompactByteString (compactHTML $ M.pageA (V @ "x") # M.pageA (V @ "x") # M.pageA (V @ "x") # M.pageA (V @ "x"))) (Put "TEST")
    ]
  ]

scaling :: Benchmark
scaling = bgroup "Scaling"
  [ bgroup "2 divs"
    [ bench "normal" $ nf (renderByteString . divs2) "input"
    , bench "compact" $ nf (renderCompactByteString . compactHTML $ divs2 (V @ "x")) (Put "input")
    ]
  , bgroup "4 divs"
    [ bench "normal" $ nf (renderByteString . divs4) "input"
    , bench "compact" $ nf (renderCompactByteString . compactHTML $ divs4 (V @ "x")) (Put "input")
    ]
  , bgroup "8 divs"
    [ bench "normal" $ nf (renderByteString . divs8) "input"
    , bench "compact" $ nf (renderCompactByteString . compactHTML $ divs8 (V @ "x")) (Put "input")
    ]
  , bgroup "16 divs"
    [ bench "normal" $ nf (renderByteString . divs16) "input"
    , bench "compact" $ nf (renderCompactByteString . compactHTML $ divs16 (V @ "x")) (Put "input")
    ]
  , bgroup "32 divs"
    [ bench "normal" $ nf (renderByteString . divs32) "input"
    , bench "compact" $ nf (renderCompactByteString . compactHTML $ divs32 (V @ "x")) (Put "input")
    ]
  ]
  where
    divs2 x = Div :> "lorem;" # x # Div :> "ipsum<>"
    divs4 = divs2 . divs2
    divs8 = divs4 . divs4
    divs16 = divs8 . divs8
    divs32 = divs16 . divs16
