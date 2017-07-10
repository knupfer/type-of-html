{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Html

import Data.String
import Data.Semigroup (Semigroup(..))
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html.Renderer.Text as RT

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "minimal"
    [ bench "strict text" $ nf (render . minimal :: T.Text -> T.Text) "TEST"
    , bench "lazy text"   $ nf (render . minimal :: LT.Text -> LT.Text) "TEST"
    , bench "builder"     $ nf (TLB.toLazyText . render . minimal :: TLB.Builder -> LT.Text) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeMinimal) "TEST"
    , bench "optimal (pregenerated lazy builder)" $ nf (TLB.toLazyText . render . bestMinimal) "TEST"
    ]
  , bgroup "hello world"
    [ bench "strict text" $ nf (render . helloWorld :: T.Text -> T.Text) "TEST"
    , bench "lazy text"   $ nf (render . helloWorld :: LT.Text -> LT.Text) "TEST"
    , bench "builder"     $ nf (TLB.toLazyText . render . helloWorld :: TLB.Builder -> LT.Text) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeHelloWorld) "TEST"
    , bench "optimal (pregenerated lazy builder)" $ nf (TLB.toLazyText . render . bestHelloWorld) "TEST"
    ]
  , bgroup "big page"
    [ bench "strict text" $ nf (render . bigPage :: T.Text -> T.Text) "TEST"
    , bench "lazy text"   $ nf (render . bigPage :: LT.Text -> LT.Text) "TEST"
    , bench "builder"     $ nf (TLB.toLazyText . render . bigPage :: TLB.Builder -> LT.Text) "TEST"
    , bench "blaze"       $ nf (RT.renderHtml . blazeBigPage) "TEST"
    , bench "optimal (pregenerated lazy builder)" $ nf (TLB.toLazyText . render . bestBigPage) "TEST"
    ]
  , bgroup "big table"
    [ bench "strict text" $ nf (render . bigTable :: (Int, Int) -> T.Text) (10,10)
    , bench "lazy text"   $ nf (render . bigTable :: (Int, Int) -> LT.Text) (10,10)
    , bench "builder"     $ nf (TLB.toLazyText . render . bigTable :: (Int, Int) -> LT.Text) (10,10)
    , bench "blaze"       $ nf (RT.renderHtml . blazeBigTable) (10,10)
    , bench "optimal (pregenerated lazy builder)" $ nf (TLB.toLazyText . render . bestBigTable) (10,10)
    ]
  ]

-- Pregenerated contents

bestMinimal :: TLB.Builder -> TLB.Builder
bestMinimal x = "<div>" <> x <> "</div>"

bestHelloWorld :: TLB.Builder -> TLB.Builder
bestHelloWorld x = "<html><head><title>" <> x <> "</title></head><body><p>Hello World!</p></body></html>"

bestBigPage :: TLB.Builder -> TLB.Builder
bestBigPage x = "<html><head><meta><title>" <> x <> "</title><script></script><link><style></style></head><body><h1><img><strong></strong></h1><div><div><img></div><div><img></div></div><div><form><fieldset><div><div><div><label></label><select><option></option><option></option></select><div></div></div><i></i></div></div><button><i></i></button></fieldset></form></div></body></html>"

bestBigTable :: (Int,Int) -> TLB.Builder
bestBigTable (n, m)
  = "<table>"
  <> stimes n ("<tr>" <> foldMap (\x -> "<td>" <> fromString (show x)) [1..m])
  <> "</table>"

-- Blaze based functions

blazeMinimal :: B.Html -> B.Html
blazeMinimal x = B.div x

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
    B.head $ do
      B.meta
      B.title x
      B.script ""
      B.link
      B.style ""
    B.body $ do
      B.h1 $ do
        B.img
        B.strong ""
      B.div $ do
        B.div B.img
        B.div B.img
      B.div $ do
        B.form $ do
          B.fieldset $ do
            B.div $ do
              B.div $ do
                B.div $ do
                  B.label ""
                  B.select $ do
                    B.option ""
                    B.option ""
                  B.div ""
                B.i ""
            B.button $ B.i ""

blazeBigTable :: (Int, Int) -> B.Html
blazeBigTable (n, m) = B.table $ replicateM_ n (B.tr $ mapM_ (B.td . fromString . show) [1..m])

-- Type of Html based functions

minimal :: ('Div ?> a) => a -> 'Div > a
minimal = div_

helloWorld :: (IsString a, 'Title ?> a) => a -> 'Html > (('Head > ('Title > a)) # ('Body > ('P > String)))
helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ ("Hello World!" :: IsString a => a)
      )
    )

--bigTypeTable :: (Enum a, Num a, Show a, Replicate n ('Tr > ['Td > String]), 'Table ?> Rep n ('Tr > ['Td > String])) => (Proxy n, a) -> 'Table > Rep n ('Tr > ['Td > String])
--bigTypeTable (n, m) = table_ $ replicateH n (tr_ $ map (td_ . show) [1..m])

bigPage :: ('Title ?> a) =>
         a
         -> 'Html
            > (('Head
                > (('Meta > ())
                   # (('Title > a)
                      # (('Script > ()) # (('Link > ()) # ('Style > ()))))))
               # ('Body
                  > (('H1 > (('Img > ()) # ('Strong > ())))
                     # (('Div > (('Div > ('Img > ())) # ('Div > ('Img > ()))))
                        # ('Div
                           > ('Form
                              > ('Fieldset
                                 > (('Div
                                     > ('Div
                                        > (('Div
                                            > (('Label > ())
                                               # (('Select
                                                   > (('Option > ())
                                                      # ('Option > ())))
                                                  # ('Div > ()))))
                                           # ('I > ()))))
                                    # ('Button > ('I > ()))))))))))
bigPage x =
  html_
    ( head_
      ( meta_ ()
      # title_ x
      # script_ ()
      # link_ ()
      # style_ ()
      )
    # body_
      ( h1_
        ( img_ ()
        # strong_ ()
        )
      # div_
        ( div_ (img_ ())
        # div_ (img_ ())
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( div_
                  ( label_ ()
                  # select_
                    ( option_ ()
                    # option_ ()
                    )
                  # div_ ()
                  )
                # i_ ()
                )
              )
            # button_ (i_ ())
            )
          )
        )
      )
    )

bigTable :: (Int, Int) -> 'Table > ['Tr > ['Td > String]]
bigTable (n, m) = table_ $ replicate n (tr_ $ map (td_ . show) [1..m])
