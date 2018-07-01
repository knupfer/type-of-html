{-# LANGUAGE DataKinds #-}

module Main where

import Html
import qualified Html.Attribute as A
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
import qualified Text.Blaze.Html5 as BL
import qualified Data.Text.Lazy   as LT
import qualified Data.Text        as T

main :: IO ()
main = defaultMain
  [ small
  , medium
  , big
  , blaze
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
  , bench "oneAttribute"                 $ nf (renderByteString . A.class_) ""
  , bench "oneAttribute ()"              $ nf (renderByteString . A.class_)  ()
  , bench "oneAttribute Proxy"           $ nf (renderByteString . A.class_)  (Proxy :: Proxy "abc")
  , bench "parallelAttribute"            $ nf (\x -> renderByteString $ A.class_ x # A.id_ x) ""
  , bench "elementWithAttribute"         $ nf (\x -> renderByteString $ div_A (A.class_ x) x) ""
  , bench "elementWithParallelAttribute" $ nf (\x -> renderByteString $ div_A (A.class_ x # A.id_ x) x) ""
  , bench "listOfAttributes"             $ nf (\x -> renderByteString [A.class_ x, A.class_ x]) ""
  , bench "listOfListOf"                 $ nf (\x -> renderByteString $ div_ [i_ [span_ x]]) ""
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

blaze :: Benchmark
blaze = bgroup "Blaze"
  [ bgroup "minimal"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeMinimal) (fromString "")
    , bench "type-of-html" $ nf (renderByteString . S.oneElement) ""
    ]
  , bgroup "hello world"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeHelloWorld) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.helloWorld) "TEST"
    ]
  , bgroup "attributes long"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeAttrLong) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.attrLong) "TEST"
    ]
  , bgroup "attributes short"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeAttrShort) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.attrShort) "TEST"
    ]
  , bgroup "page"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazePage) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.page) "TEST"
    ]
  , bgroup "page with attributes"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazePageA) (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . M.pageA) "TEST"
    ]
  , bgroup "table"
    [ bench "blaze-html"   $ nf (renderHtml . BL.blazeTable) (4,4)
    , bench "type-of-html" $ nf (renderByteString . M.table) (4,4)
    ]
  , bgroup "encode random string"
    [ bench "blaze-html"   $ nf (renderHtml . BL.div . BL.toHtml) randomString
    , bench "type-of-html" $ nf (renderByteString . div_) randomString
    ]
  ]
