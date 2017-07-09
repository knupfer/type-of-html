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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html.Renderer.Utf8 as RU
import qualified Text.Blaze.Html.Renderer.Text as RT

import Criterion.Main

main :: IO ()
main = defaultMain
  [ let run f = nf (f :: String -> String) "TEST"
    in bgroup "string"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (render . bigTable :: (Int, Int) -> String) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (bestBigTable  :: (Int, Int) -> String) (10, 10)
      ]
    ]

  , let run f = nf (f :: T.Text -> T.Text) "TEST"
    in bgroup "strict text"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (render . bigTable :: (Int, Int) -> T.Text) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (bestBigTable  :: (Int, Int) -> T.Text) (10, 10)
      ]
    ]

  , let run f = nf (f :: LT.Text -> LT.Text) "TEST"
    in bgroup "lazy text"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (render . bigTable :: (Int, Int) -> LT.Text) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (bestBigTable  :: (Int, Int) -> LT.Text) (10, 10)
      ]
    ]

  , let run f = nf (TLB.toLazyText . f :: TLB.Builder -> LT.Text) "TEST"
    in bgroup "lazy builder"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (TLB.toLazyText . render . bigTable :: (Int, Int) -> LT.Text) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (TLB.toLazyText . bestBigTable  :: (Int, Int) -> LT.Text) (10, 10)
      ]
    ]

  , let run f = nf (f :: BS8.ByteString -> BS8.ByteString) "TEST"
    in bgroup "strict bytestring"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (render . bigTable :: (Int,Int) -> BS8.ByteString) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (bestBigTable  :: (Int, Int) -> BS8.ByteString) (10, 10)
      ]
    ]

  , let run f = nf (f :: LBS8.ByteString -> LBS8.ByteString) "TEST"
    in bgroup "lazy bytestring"
    [ bgroup "generated"
      [ bench "minimal"     $ run (render . minimal)
      , bench "hello world" $ run (render . helloWorld)
      , bench "big page"    $ run (render . bigPage)
      , bench "big table"   $ nf (render . bigTable :: (Int, Int) -> LBS8.ByteString) (10, 10)
      ]
    , bgroup "perfect"
      [ bench "minimal"     $ run bestMinimal
      , bench "hello world" $ run bestHelloWorld
      , bench "big page"    $ run bestBigPage
      , bench "big table"   $ nf (bestBigTable  :: (Int, Int) -> LBS8.ByteString) (10, 10)
      ]
    ]

  , bgroup "blaze html (bytestring)"
    [ bgroup "generated"
      [ bench "minimal"     $ nf (RU.renderHtml . blazeMinimal) "TEST"
      , bench "hello world" $ nf (RU.renderHtml . blazeHelloWorld) "TEST"
      , bench "big page"    $ nf (RU.renderHtml . blazeBigPage) "TEST"
      , bench "big table"   $ nf (RU.renderHtml . blazeBigTable) (10,10)
      ]
    ]
  , bgroup "blaze html (text)"
    [ bgroup "generated"
      [ bench "minimal"     $ nf (RT.renderHtml . blazeMinimal) "TEST"
      , bench "hello world" $ nf (RT.renderHtml . blazeHelloWorld) "TEST"
      , bench "big page"    $ nf (RT.renderHtml . blazeBigPage) "TEST"
      , bench "big table"   $ nf (RT.renderHtml . blazeBigTable) (10,10)
      ]
    ]
  ]

blazeBigTable :: (Int, Int) -> B.Html
blazeBigTable (n, m) = B.table $ replicateM_ n (B.tr $ mapM_ (B.td . fromString . show) [1..m])


bigTable :: (Int, Int) -> 'Table > ['Tr > ['Td > String]]
bigTable (n, m) = table_ $ replicate n (tr_ $ map (td_ . show) [1..m])

bestBigTable :: (IsString a, Semigroup a, Monoid a) => (Int,Int) -> a
bestBigTable (n, m)
  = "<table>"
  <> stimes n ("<tr>" <> foldMap (\x -> "<td>" <> fromString (show x)) [1..m])
  <> "</table>"

--bigTypeTable :: (Enum a, Num a, Show a, Replicate n ('Tr > ['Td > String]), 'Table ?> Rep n ('Tr > ['Td > String])) => (Proxy n, a) -> 'Table > Rep n ('Tr > ['Td > String])
--bigTypeTable (n, m) = table_ $ replicateH n (tr_ $ map (td_ . show) [1..m])

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

bestMinimal :: (IsString a, Semigroup a) => a -> a
bestMinimal x = "<div>" <> x <> "</div>"

bestHelloWorld :: (IsString a, Semigroup a) => a -> a
bestHelloWorld x = "<html><head><title>" <> x <> "</title></head><body><p>Hello World!</p></body></html>"

bestBigPage :: (IsString a, Semigroup a) => a -> a
bestBigPage x = "<html><head><meta><title>" <> x <> "</title><script></script><link><style></style></head><body><h1><img><strong></strong></h1><div><div><img></div><div><img></div></div><div><form><fieldset><div><div><div><label></label><select><option></option><option></option></select><div></div></div><i></i></div></div><button><i></i></button></fieldset></form></div></body></html>"

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
