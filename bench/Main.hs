{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}

module Main where

import Html
import qualified Html.Attribute as A

import Data.String
import Control.Monad
import Criterion.Main
import Data.Monoid

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Utf8

import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as BA

main :: IO ()
main = defaultMain
  [ bgroup "minimal"
    [ bench "blaze-html"   $ nf (renderHtml . blazeMinimal)     (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . minimal)    "TEST"
    ]
  , bgroup "hello world"
    [ bench "blaze-html"   $ nf (renderHtml . blazeHelloWorld)  (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . helloWorld) "TEST"
    ]
  , bgroup "attributes short"
    [ bench "blaze-html"   $ nf (renderHtml . blazeAttrShort)   (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . attrShort)  "TEST"
    ]
  , bgroup "attributes long"
    [ bench "blaze-html"   $ nf (renderHtml . blazeAttrLong)    (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . attrLong)   "TEST"
    ]
  , bgroup "big page"
    [ bench "blaze-html"   $ nf (renderHtml . blazeBigPage)     (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . bigPage)    "TEST"
    ]
  , bgroup "big page with attributes"
    [ bench "blaze-html"   $ nf (renderHtml . blazeBigPageA)    (fromString "TEST")
    , bench "type-of-html" $ nf (renderByteString . bigPageA)   "TEST"
    ]
  , bgroup "big table"
    [ bench "blaze-html"   $ nf (renderHtml . blazeBigTable)    (4,4)
    , bench "type-of-html" $ nf (renderByteString . bigTable)   (4,4)
    ]
  ]

-- Blaze based functions

blazeMinimal :: B.Html -> B.Html
blazeMinimal = B.div

blazeHelloWorld :: B.Html -> B.Html
blazeHelloWorld x =
  B.html $ do
    B.head $ do
      B.title x
    B.body $ do
      B.p $ fromString "Hello World!"

blazeBigPage :: B.Html -> B.Html
blazeBigPage x =
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
  = B.i ! BA.accept (fromString "a")
  $ B.i ! BA.acceptCharset (fromString "b")
  $ B.i ! BA.accesskey (fromString "c")
  $ B.i ! BA.action (fromString "d")
  $ B.i ! BA.alt (fromString "f")
  $ B.i ! BA.async (fromString "g")
  $ x

blazeAttrLong :: B.Html -> B.Html
blazeAttrLong x
  = B.i ! BA.accept (fromString "a")
        ! BA.acceptCharset (fromString "b")
        ! BA.accesskey (fromString "c")
        ! BA.action (fromString "d")
        ! BA.alt (fromString "f")
        ! BA.async (fromString "g")
  $ x

blazeBigPageA :: B.Html -> B.Html
blazeBigPageA x =
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

blazeBigTable :: (Int, Int) -> B.Html
blazeBigTable (n, m)
  = B.table
  . replicateM_ n
  . B.tr
  $ mapM_ (B.td . fromString . show) [1..m]

-- Type of Html based functions

minimal = div_

helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ "Hello World!"
      )
    )

bigPage x =
  html_
    ( body_
      ( h1_
        ( img_
        # strong_ (0 :: Int)
        )
      # div_
        ( div_ (1 :: Int)
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( label_ "a"
                # select_
                  ( option_ "b"
                  # option_ "c"
                  )
                # div_ "d"
                )
              # i_ x
              )
            # button_ (i_ "e")
            )
          )
        )
      )
    )

attrLong x =
  i_A ( A.accept_ "a"
     <> A.acceptcharset_ "b"
     <> A.accesskey_ "c"
     <> A.action_ "d"
     <> A.alt_ "f"
     <> A.async_ "g") x

attrShort x
  = i_A (A.accept_ "a")
  . i_A (A.acceptcharset_ "b")
  . i_A (A.accesskey_ "c")
  . i_A (A.action_ "d")
  . i_A (A.alt_ "f")
  $ i_A (A.async_ "g") x


bigPageA x =
  html_
    ( body_
      ( h1_A (A.id_ "a")
        ( img_
        # strong_A (A.class_ "b") (0 :: Int)
        )
      # div_
        ( div_A (A.id_ "c") (1 :: Int)
        )
      # div_
        ( form_A (A.class_ "d")
          ( fieldset_
            ( div_A (A.id_ "e")
              ( div_
                ( label_A (A.class_ "f") "a"
                # select_
                  ( option_A (A.id_ "g") "b"
                  # option_ "c"
                  )
                # div_A (A.class_ "h") "d"
                )
              # i_ x
              )
            # button_A (A.id_ "i") (i_ "e")
            )
          )
        )
      )
    )

bigTable (n, m) = table_ . replicate n . tr_ $ map td_ [(1::Int)..m]
