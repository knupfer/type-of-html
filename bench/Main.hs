{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Html

import Data.String
import Control.Monad
import Criterion.Main

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.Utf8

import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as B (class_, id)

main :: IO ()
main = defaultMain
  [ bgroup "minimal"
    [ bench "blaze.utf8" $ nf (renderHtml . blazeMinimal)     (fromString "TEST")
    , bench "string"     $ nf (renderString . minimal)        "TEST"
    , bench "bytestring" $ nf (renderByteString . minimal)    "TEST"
    , bench "text"       $ nf (renderText . minimal)          "TEST"
    ]
  , bgroup "hello world"
    [ bench "blaze.utf8" $ nf (renderHtml . blazeHelloWorld)  (fromString "TEST")
    , bench "string"     $ nf (renderString . helloWorld)     "TEST"
    , bench "bytestring" $ nf (renderByteString . helloWorld) "TEST"
    , bench "text"       $ nf (renderText . helloWorld)       "TEST"
    ]
  , bgroup "big page"
    [ bench "blaze.utf8" $ nf (renderHtml . blazeBigPage)     (fromString "TEST")
    , bench "string"     $ nf (renderString . bigPage)        "TEST"
    , bench "bytestring" $ nf (renderByteString . bigPage)    "TEST"
    , bench "text"       $ nf (renderText . bigPage)          "TEST"
    ]
  , bgroup "big page with attributes"
    [ bench "blaze.utf8" $ nf (renderHtml . blazeBigPageA)    (fromString "TEST")
    , bench "string"     $ nf (renderString . bigPageA)       "TEST"
    , bench "bytestring" $ nf (renderByteString . bigPageA)   "TEST"
    , bench "text"       $ nf (renderText . bigPageA)         "TEST"
    ]
  , bgroup "big table"
    [ bench "blaze.utf8" $ nf (renderHtml . blazeBigTable)    (4,4)
    , bench "string"     $ nf (renderString . bigTable)       (4,4)
    , bench "bytestring" $ nf (renderByteString . bigTable)   (4,4)
    , bench "text"       $ nf (renderText . bigTable)         (4,4)
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

blazeBigPageA :: B.Html -> B.Html
blazeBigPageA x =
  B.html $ do
    B.body $ do
      B.h1 ! B.id (fromString "a") $ do
        B.img
        B.strong ! B.class_ (fromString "b") $ fromString "0"
      B.div $ do
        B.div ! B.id (fromString "c") $ fromString "1"
      B.div $ do
        B.form ! B.class_ (fromString "d") $ do
          B.fieldset $ do
            B.div ! B.id (fromString "e") $ do
              B.div $ do
                B.label ! B.class_ (fromString "f") $ fromString "a"
                B.select $ do
                  B.option ! B.id (fromString "g") $ fromString "b"
                  B.option (fromString "c")
                B.div ! B.class_ (fromString "h") $ fromString "d"
              B.i x
            B.button ! B.id (fromString "i") $ B.i $ fromString "e"

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

bigPageA x =
  html_
    ( body_
      ( h1_A [("id","a")]
        ( img_
        # strong_A [("class","b")] (0 :: Int)
        )
      # div_
        ( div_A [("id","c")] (1 :: Int)
        )
      # div_
        ( form_A [("class","d")]
          ( fieldset_
            ( div_A [("id","e")]
              ( div_
                ( label_A [("class","f")] "a"
                # select_
                  ( option_A [("id","g")] "b"
                  # option_ "c"
                  )
                # div_A [("class","h")] "d"
                )
              # i_ x
              )
            # button_A [("id","i")] (i_ "e")
            )
          )
        )
      )
    )

bigTable (n, m) = table_ . replicate n . tr_ $ map td_ [(1::Int)..m]
