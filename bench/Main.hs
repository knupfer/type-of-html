{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Html

import Data.Proxy
import Data.String
import Control.Monad

import Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L
import qualified Text.Blaze.Html5              as B
import qualified Text.Blaze.Html.Renderer.Text as RT

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "minimal"
    [ bench "string"      $ nf (render . minimal                   :: String         -> String) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeMinimal       :: B.Html         -> L.Text) "TEST"
    , bench "strict text" $ nf (render . minimal                   :: S.Text         -> S.Text) "TEST"
    , bench "lazy text"   $ nf (render . minimal                   :: L.Text         -> L.Text) "TEST"
    , bench "builder"     $ nf (toLazyText . render . minimal      :: Builder        -> L.Text) "TEST"
    ]
  , bgroup "hello world"
    [ bench "string"      $ nf (render . helloWorld                :: String         -> String) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeHelloWorld    :: B.Html         -> L.Text) "TEST"
    , bench "strict text" $ nf (render . helloWorld                :: S.Text         -> S.Text) "TEST"
    , bench "lazy text"   $ nf (render . helloWorld                :: L.Text         -> L.Text) "TEST"
    , bench "builder"     $ nf (toLazyText . render . helloWorld   :: Builder        -> L.Text) "TEST"
    ]
  , bgroup "big page"
    [ bench "string"      $ nf (render . bigPage                   :: String         -> String) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeBigPage       :: B.Html         -> L.Text) "TEST"
    , bench "strict text" $ nf (render . bigPage                   :: S.Text         -> S.Text) "TEST"
    , bench "lazy text"   $ nf (render . bigPage                   :: L.Text         -> L.Text) "TEST"
    , bench "builder"     $ nf (toLazyText . render . bigPage      :: Builder        -> L.Text) "TEST"
    ]
  , bgroup "big table"
    [ bench "string"      $ nf (render . bigTable                  :: (Int, Int)     -> String) (5,5)
    , bench "blaze"       $ nf (RT.renderHtml . blazeBigTable      :: (Int, Int)     -> L.Text) (5,5)
    , bench "strict text" $ nf (render . bigTable                  :: (Int, Int)     -> S.Text) (5,5)
    , bench "lazy text"   $ nf (render . bigTable                  :: (Int, Int)     -> L.Text) (5,5)
    , bench "builder"     $ nf (toLazyText . render . bigTable     :: (Int, Int)     -> L.Text) (5,5)
    ]
  , bgroup "type table"
    [ bench "string"      $ nf (render . bigTypeTable              :: (Proxy 5, Int) -> String) (Proxy,5)
    , bench "strict text" $ nf (render . bigTypeTable              :: (Proxy 5, Int) -> S.Text) (Proxy,5)
    , bench "lazy text"   $ nf (render . bigTypeTable              :: (Proxy 5, Int) -> L.Text) (Proxy,5)
    , bench "builder"     $ nf (toLazyText . render . bigTypeTable :: (Proxy 5, Int) -> L.Text) (Proxy,5)
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

bigTable :: (Int, Int) -> 'Table > ['Tr > ['Td > Int]]
bigTable (n, m) = table_ . replicate n . tr_ $ map td_ [1..m]

bigTypeTable
  :: ( Replicate n ('Tr > ['Td > Int])
     , 'Table ?> Rep n ('Tr > ['Td > Int])
     )
  => (Proxy n, Int)
  -> 'Table > Rep n ('Tr > ['Td > Int])
bigTypeTable (n, m) = table_ . replicateH n . tr_ $ map td_ [1..m]
