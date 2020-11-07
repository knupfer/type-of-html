{-# LANGUAGE OverloadedStrings #-}

module Blaze where

import Control.Monad
import Text.Blaze.Html5 ((!))

import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as BA

blazeMinimal :: B.Html -> B.Html
blazeMinimal = B.div

blazeHelloWorld :: B.Html -> B.Html
blazeHelloWorld x =
  B.html $ do
    B.head $ do
      B.title x
    B.body $ do
      B.p "Hello World!"

blazePage :: B.Html -> B.Html
blazePage x =
  B.html $ do
    B.body $ do
      B.h1 $ do
        B.img
        B.strong (B.toHtml (0 :: Int))
      B.div $ do
        B.div (B.toHtml (1 :: Int))
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

blazeAttrShort :: B.AttributeValue -> B.Html
blazeAttrShort x
  = B.i ! BA.accesskey       "a"
  $ B.i ! BA.class_          "b"
  $ B.i ! BA.contenteditable "c"
  $ B.i ! BA.contextmenu     "d"
  $ B.i ! BA.dir             "e"
  $ B.i ! BA.draggable       "f"
  $ B.i ! BA.hidden          mempty
  $ B.i ! BA.id              "h"
  $ B.i ! BA.itemprop        "i"
  $ B.i ! BA.lang            "j"
  $ B.i ! BA.spellcheck      "k"
  $ B.i ! BA.style           "l"
  $ B.i ! BA.title           x
  $ "m"

blazeAttrLong :: B.AttributeValue -> B.Html
blazeAttrLong x
  = B.i ! BA.accesskey       "a"
        ! BA.class_          "b"
        ! BA.contenteditable "c"
        ! BA.contextmenu     "d"
        ! BA.dir             "e"
        ! BA.draggable       "f"
        ! BA.hidden          mempty
        ! BA.id              "h"
        ! BA.itemprop        "i"
        ! BA.lang            "j"
        ! BA.spellcheck      "k"
        ! BA.style           "l"
        ! BA.title           x
        $ "m"

blazePageA :: B.Html -> B.Html
blazePageA x =
  B.html $ do
    B.body $ do
      B.h1 ! BA.id "a" $ do
        B.img
        B.strong ! BA.class_ "b" $ (B.toHtml (0 :: Int))
      B.div $ do
        B.div ! BA.id "c" $ (B.toHtml (1 :: Int))
      B.div $ do
        B.form ! BA.class_ "d" $ do
          B.fieldset $ do
            B.div ! BA.id "e" $ do
              B.div $ do
                B.label ! BA.class_ "f" $ "a"
                B.select $ do
                  B.option ! BA.id "g" $ "b"
                  B.option "c"
                B.div ! BA.class_ "h" $ "d"
              B.i x
            B.button ! BA.id "i" $ do
              B.i "e"

blazeTable :: (Int, Int) -> B.Html
blazeTable (n, m)
  = B.table
  . replicateM_ n
  . B.tr
  $ mapM_ (B.td . B.toHtml) [1..m]
