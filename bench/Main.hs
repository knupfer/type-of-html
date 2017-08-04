{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Html

import Data.String
import Control.Monad

import Data.Text.Lazy.Builder (Builder, toLazyText)
import Text.Blaze.Html5 ((!))

import Data.ByteString.Lazy (ByteString)

import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L
import qualified Text.Blaze.Html5              as B
import qualified Text.Blaze.Html5.Attributes   as B (class_, id)
import qualified Text.Blaze.Html.Renderer.Text as RT
import qualified Text.Blaze.Html.Renderer.Utf8 as RB

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "minimal"
    [ bench "blaze.text"  $ nf (RT.renderHtml . blazeMinimal     :: B.Html     -> L.Text)     "TEST"
    , bench "blaze.utf8"  $ nf (RB.renderHtml . blazeMinimal     :: B.Html     -> ByteString) "TEST"
    , bench "string"      $ nf (render . minimal                 :: String     -> String)     "TEST"
    , bench "strict text" $ nf (render . minimal                 :: S.Text     -> S.Text)     "TEST"
    , bench "lazy text"   $ nf (render . minimal                 :: L.Text     -> L.Text)     "TEST"
    , bench "builder"     $ nf (toLazyText . render . minimal    :: Builder    -> L.Text)     "TEST"
    ]
  , bgroup "hello world"
    [ bench "blaze.text"  $ nf (RT.renderHtml . blazeHelloWorld  :: B.Html     -> L.Text)     "TEST"
    , bench "blaze.utf8"  $ nf (RB.renderHtml . blazeHelloWorld  :: B.Html     -> ByteString) "TEST"
    , bench "string"      $ nf (render . helloWorld              :: String     -> String)     "TEST"
    , bench "strict text" $ nf (render . helloWorld              :: S.Text     -> S.Text)     "TEST"
    , bench "lazy text"   $ nf (render . helloWorld              :: L.Text     -> L.Text)     "TEST"
    , bench "builder"     $ nf (toLazyText . render . helloWorld :: Builder    -> L.Text)     "TEST"
    ]
  , bgroup "big page"
    [ bench "blaze.text"  $ nf (RT.renderHtml . blazeBigPage     :: B.Html     -> L.Text)     "TEST"
    , bench "blaze.utf8"  $ nf (RB.renderHtml . blazeBigPage     :: B.Html     -> ByteString) "TEST"
    , bench "string"      $ nf (render . bigPage                 :: String     -> String)     "TEST"
    , bench "strict text" $ nf (render . bigPage                 :: S.Text     -> S.Text)     "TEST"
    , bench "lazy text"   $ nf (render . bigPage                 :: L.Text     -> L.Text)     "TEST"
    , bench "builder"     $ nf (toLazyText . render . bigPage    :: Builder    -> L.Text)     "TEST"
    ]
  , bgroup "big page with attributes"
    [ bench "blaze.text"  $ nf (RT.renderHtml . blazeBigPageA    :: B.Html     -> L.Text)     "TEST"
    , bench "blaze.utf8"  $ nf (RB.renderHtml . blazeBigPageA    :: B.Html     -> ByteString) "TEST"
    , bench "string"      $ nf (render . bigPageA                :: String     -> String)     "TEST"
    , bench "strict text" $ nf (render . bigPageA                :: S.Text     -> S.Text)     "TEST"
    , bench "lazy text"   $ nf (render . bigPageA                :: L.Text     -> L.Text)     "TEST"
    , bench "builder"     $ nf (toLazyText . render . bigPageA   :: Builder    -> L.Text)     "TEST"
    ]
  , bgroup "big table"
    [ bench "blaze.text"  $ nf (RT.renderHtml . blazeBigTable    :: (Int, Int) -> L.Text)     (4,4)
    , bench "blaze.utf8"  $ nf (RB.renderHtml . blazeBigTable    :: (Int, Int) -> ByteString) (4,4)
    , bench "string"      $ nf (render . bigTable                :: (Int, Int) -> String)     (4,4)
    , bench "strict text" $ nf (render . bigTable                :: (Int, Int) -> S.Text)     (4,4)
    , bench "lazy text"   $ nf (render . bigTable                :: (Int, Int) -> L.Text)     (4,4)
    , bench "builder"     $ nf (toLazyText . render . bigTable   :: (Int, Int) -> L.Text)     (4,4)
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
      B.p "Hello World!"

blazeBigPage :: B.Html -> B.Html
blazeBigPage x =
  B.html $ do
    B.body $ do
      B.h1 $ do
        B.img
        B.strong "0"
      B.div $ do
        B.div "1"
      B.div $ do
        B.form $ do
          B.fieldset $ do
            B.div $ do
              B.div $ do
                B.label "a"
                B.select $ do
                  B.option "b"
                  B.option "c"
                B.div "d"
              B.i x
            B.button $ B.i "e"

blazeBigPageA :: B.Html -> B.Html
blazeBigPageA x =
  B.html $ do
    B.body $ do
      B.h1 ! B.id "a" $ do
        B.img
        B.strong ! B.class_ "b" $ "0"
      B.div $ do
        B.div ! B.id "c" $ "1"
      B.div $ do
        B.form ! B.class_ "d" $ do
          B.fieldset $ do
            B.div ! B.id "e" $ do
              B.div $ do
                B.label ! B.class_ "f" $ "a"
                B.select $ do
                  B.option ! B.id "g" $ "b"
                  B.option "c"
                B.div ! B.class_ "h" $ "d"
              B.i x
            B.button ! B.id "i" $ B.i "e"

blazeBigTable :: (Int, Int) -> B.Html
blazeBigTable (n, m)
  = B.table
  . replicateM_ n
  . B.tr
  $ mapM_ (B.td . fromString . show) [1..m]

-- Type of Html based functions

minimal :: ('Div ?> a) => a -> 'Div > a
minimal = div_

helloWorld
  :: (IsString a, 'Title ?> a)
  => a
  ->
  'Html > ( 'Head > 'Title > a
          # 'Body > 'P > String
          )
helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ ("Hello World!" :: String)
      )
    )

bigPage
  :: ('I ?> a)
  => a
  ->
  'Html >
  ( 'Body >
    ( 'H1 >
      ( 'Img > ()
      # 'Strong > String
      )
    # 'Div > 'Div > String
    # 'Div > 'Form > 'Fieldset >
      ( 'Div >
        ( 'Div >
          ( 'Label > String
          # 'Select >
            ( 'Option > String
            # 'Option > String
            )
          # 'Div > String
          )
        # 'I > a
        )
      # 'Button > 'I > String
      )
    )
  )
bigPage x =
  html_
    ( body_
      ( h1_
        ( img_
        # strong_ ("0" :: String)
        )
      # div_
        ( div_ ("1" :: String)
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( label_ ("a" :: String)
                # select_
                  ( option_ ("b" :: String)
                  # option_ ("c" :: String)
                  )
                # div_ ("d" :: String)
                )
              # i_ x
              )
            # button_ (i_ ("e" :: String))
            )
          )
        )
      )
    )

bigPageA
  :: ('I ?> a)
  => a
  ->
  'Html >
  ( 'Body >
    ( 'H1 :>
      ( 'Img > ()
      # 'Strong :> String
      )
    # 'Div > 'Div :> String
    # 'Div > 'Form :> 'Fieldset >
      ( 'Div :>
        ( 'Div >
          ( 'Label :> String
          # 'Select >
            ( 'Option :> String
            # 'Option > String
            )
          # 'Div :> String
          )
        # 'I > a
        )
      # 'Button :> 'I > String
      )
    )
  )
bigPageA x =
  html_
    ( body_
      ( h1_A [("id","a")]
        ( img_
        # strong_A [("class","b")] ("0" :: String)
        )
      # div_
        ( div_A [("id","c")] ("1" :: String)
        )
      # div_
        ( form_A [("class","d")]
          ( fieldset_
            ( div_A [("id","e")]
              ( div_
                ( label_A [("class","f")] ("a" :: String)
                # select_
                  ( option_A [("id","g")] ("b" :: String)
                  # option_ ("c" :: String)
                  )
                # div_A [("class","h")] ("d" :: String)
                )
              # i_ x
              )
            # button_A [("id","i")] (i_ ("e" :: String))
            )
          )
        )
      )
    )

bigTable :: (Int, Int) -> 'Table > ['Tr > ['Td > Int]]
bigTable (n, m) = table_ . replicate n . tr_ $ map td_ [1..m]
