{-# LANGUAGE OverloadedStrings #-}

module Blaze where

import Control.Monad
import Text.Blaze.Html5 ((!))

import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as BA

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
