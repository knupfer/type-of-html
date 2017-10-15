module Blaze where

import Html
import Small
import Medium

import Data.String
import Criterion.Main
import Control.Monad
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Utf8
import System.IO.Unsafe
import Test.QuickCheck

import qualified Data.Text as T

import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as BA

{-# NOINLINE randomText #-}
randomText :: T.Text
randomText = unsafePerformIO $ do
  s <- take 250 <$> generate infiniteList :: IO String
  return $ T.pack s

blaze :: Benchmark
blaze = bgroup "Blaze"
  [ bgroup "minimal"
    [ bench "blaze-html"   $ nf (renderHtml . blazeMinimal)     (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . oneElement) "TEST"
    ]
  , bgroup "hello world"
    [ bench "blaze-html"   $ nf (renderHtml . blazeHelloWorld)  (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . helloWorld) "TEST"
    ]
  , bgroup "attributes long"
    [ bench "blaze-html"   $ nf (renderHtml . blazeAttrLong)    (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . attrLong)   "TEST"
    ]
  , bgroup "attributes short"
    [ bench "blaze-html"   $ nf (renderHtml . blazeAttrShort)   (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . attrShort)  "TEST"
    ]
  , bgroup "page"
    [ bench "blaze-html"   $ nf (renderHtml . blazePage)        (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . page)       "TEST"
    ]
  , bgroup "page with attributes"
    [ bench "blaze-html"   $ nf (renderHtml . blazePageA)       (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . pageA)      "TEST"
    ]
  , bgroup "table"
    [ bench "blaze-html"   $ nf (renderHtml . blazeTable)       (4,4)
    , bench "type-of-html" $ nf (renderByteString . table)      (4,4)
    ]
  , bgroup "encode strict text"
    [ bench "blaze-html"   $ nf (renderHtml . B.div . B.toHtml) randomText
    , bench "type-of-html" $ nf (renderByteString . div_) randomText
    ]
  ]

blazeMinimal :: B.Html -> B.Html
blazeMinimal = B.div

blazeHelloWorld :: B.Html -> B.Html
blazeHelloWorld x =
  B.html $ do
    B.head $ do
      B.title x
    B.body $ do
      B.p $ fromString "Hello World!"

blazePage :: B.Html -> B.Html
blazePage x =
  B.html $ do
    B.body $ do
      B.h1 $ do
        B.img
        B.strong $ fromString "0"
      B.div $ do
        B.div $ fromString "1"
      B.div $ do
        B.form $ do
          B.fieldset $ do
            B.div $ do
              B.div $ do
                B.label $ fromString "a"
                B.select $ do
                  B.option $ fromString "b"
                  B.option $ fromString "c"
                B.div $ fromString "d"
              B.i x
            B.button . B.i $ fromString "e"

blazeAttrShort :: B.Html -> B.Html
blazeAttrShort x
  = B.i ! BA.accesskey       (fromString "a")
  $ B.i ! BA.class_          (fromString "b")
  $ B.i ! BA.contenteditable (fromString "c")
  $ B.i ! BA.contextmenu     (fromString "d")
  $ B.i ! BA.dir             (fromString "e")
  $ B.i ! BA.draggable       (fromString "f")
  $ B.i ! BA.hidden          (fromString "g")
  $ B.i ! BA.id              (fromString "h")
  $ B.i ! BA.itemprop        (fromString "i")
  $ B.i ! BA.lang            (fromString "j")
  $ B.i ! BA.spellcheck      (fromString "k")
  $ B.i ! BA.style           (fromString "l")
  $ B.i ! BA.tabindex        (fromString "m")
  $ B.i ! BA.title           (fromString "n")
  $ x

blazeAttrLong :: B.Html -> B.Html
blazeAttrLong x
  = B.i ! BA.accesskey       (fromString "a")
        ! BA.class_          (fromString "b")
        ! BA.contenteditable (fromString "c")
        ! BA.contextmenu     (fromString "d")
        ! BA.dir             (fromString "e")
        ! BA.draggable       (fromString "f")
        ! BA.hidden          (fromString "g")
        ! BA.id              (fromString "h")
        ! BA.itemprop        (fromString "i")
        ! BA.lang            (fromString "j")
        ! BA.spellcheck      (fromString "k")
        ! BA.style           (fromString "l")
        ! BA.tabindex        (fromString "m")
        ! BA.title           (fromString "n")
        $ x

blazePageA :: B.Html -> B.Html
blazePageA x =
  B.html $ do
    B.body $ do
      B.h1 ! BA.id (fromString "a") $ do
        B.img
        B.strong ! BA.class_ (fromString "b") $ fromString "0"
      B.div $ do
        B.div ! BA.id (fromString "c") $ fromString "1"
      B.div $ do
        B.form ! BA.class_ (fromString "d") $ do
          B.fieldset $ do
            B.div ! BA.id (fromString "e") $ do
              B.div $ do
                B.label ! BA.class_ (fromString "f") $ fromString "a"
                B.select $ do
                  B.option ! BA.id (fromString "g") $ fromString "b"
                  B.option (fromString "c")
                B.div ! BA.class_ (fromString "h") $ fromString "d"
              B.i x
            B.button ! BA.id (fromString "i") $ B.i $ fromString "e"

blazeTable :: (Int, Int) -> B.Html
blazeTable (n, m)
  = B.table
  . replicateM_ n
  . B.tr
  $ mapM_ (B.td . fromString . show) [1..m]

