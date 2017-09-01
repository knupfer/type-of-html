{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html

import Data.String
import Data.Proxy
import Test.Hspec

import Data.Text.Lazy.Encoding

import qualified Data.Text.Lazy as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = let
           allT a b c = b (renderString a, renderText a, renderByteString a)
                          (c, fromString c, fromString c)

       in parallel $ do

  describe "render" $ do

    it "is id on empty string" $ do

      allT ""
        shouldBe
        ""

    it "handles single elements" $ do

      allT (div_ "a")
        shouldBe
        "<div>a</div>"

    it "handles nested elements" $ do

      allT (div_ (div_ "a"))
        shouldBe
        "<div><div>a</div></div>"

    it "handles parallel elements" $ do

      allT (div_ "a" # div_ "b")
        shouldBe
        "<div>a</div><div>b</div>"

    it "doesn't use closing tags for empty elements" $ do

      allT area_
        shouldBe
        "<area>"

      allT base_
        shouldBe
        "<base>"

      allT br_
        shouldBe
        "<br>"

      allT col_
        shouldBe
        "<col>"

      allT embed_
        shouldBe
        "<embed>"

      allT hr_
        shouldBe
        "<hr>"

      allT iframe_
        shouldBe
        "<iframe>"

      allT img_
        shouldBe
        "<img>"

      allT link_
        shouldBe
        "<link>"

      allT menuitem_
        shouldBe
        "<menuitem>"

      allT meta_
        shouldBe
        "<meta>"

      allT param_
        shouldBe
        "<param>"

      allT source_
        shouldBe
        "<source>"

      allT track_
        shouldBe
        "<track>"

      allT wbr_
        shouldBe
        "<wbr>"

    it "avoids optional closing tags" $ do

      -- The closing tag at the end is because we can't know what
      -- element will follow.

      allT (td_ () # td_ ())
        shouldBe
        "<td><td></td>"

      allT (tr_ $ td_ ())
        shouldBe
        "<tr><td></tr>"

      allT (table_ . tr_ $ td_ ())
        shouldBe
        "<table><tr><td></table>"

    it "handles trailing text" $ do

      allT (td_ "a" # "b")
        shouldBe
        "<td>a</td>b"

    it "handles a single compile time text" $ do

      allT (Proxy :: Proxy "a")
        shouldBe
        "a"

    it "handles trailing compile time text" $ do

      allT (div_ "a" # (Proxy :: Proxy "b"))
        shouldBe
        "<div>a</div>b"

    it "handles nested compile time text" $ do

      allT (div_ (Proxy :: Proxy "a"))
        shouldBe
        "<div>a</div>"

    it "handles an empty list" $ do

      allT (tail [td_ "a"])
        shouldBe
        ""

    it "handles a list with a single element" $ do

      allT [td_ "a"]
        shouldBe
        "<td>a</td>"

    it "handles tags in a list with parallel elements" $ do

      allT [td_ "a" # td_ "b"]
        shouldBe
        "<td>a<td>b</td>"

    it "handles tags in a list with parallel elements and a following tag" $ do

      pendingWith "This is a not yet implemented optimization"

      allT ([td_ "a" # td_ "b"] # td_ "c")
        shouldBe
        "<td>a<td>b<td>c</td>"

      allT ([td_ "a" # td_ "b"] # div_ "c")
        shouldBe
        "<td>a<td>b</td><div>c</div>"

      allT ([div_ "a" # td_ "b"] # td_ "c")
        shouldBe
        "<div>a</div><td>b</td><td>c</td>"

    it "handles tags in a list when the list is the last child" $ do

      allT (tr_ [td_ "a" # td_ "b"])
        shouldBe
        "<tr><td>a<td>b</tr>"

    it "handles nested lists" $ do

      allT (table_ [tr_ [td_ (4 :: Int)]])
        shouldBe
        "<table><tr><td>4</table>"

    it "handles tags before a list" $ do

      pendingWith "This is a not yet implemented optimization"

      allT (td_ "a" # [td_ "b"] # table_ ())
        shouldBe
        "<td>a</td><td>b</td><table></table>"

      allT (td_ "a" # [td_ "b"] # td_ "c")
        shouldBe
        "<td>a<td>b<td>c</td>"

    it "handles utf8 correctly" $ do

      renderString (div_ "äöüß")
        `shouldBe`
        "<div>äöüß</div>"

      T.unpack (renderText (div_ "äöüß"))
        `shouldBe`
        "<div>äöüß</div>"

      T.unpack (decodeUtf8 (renderByteString (div_ "äöüß")))
        `shouldBe`
        "<div>äöüß</div>"

      renderString (img_A [("id","äöüß")])
        `shouldBe`
        "<img id=\"äöüß\">"

      T.unpack (renderText (img_A [("id","äöüß")]))
        `shouldBe`
        "<img id=\"äöüß\">"

      T.unpack (decodeUtf8 (renderByteString (img_A [("id","äöüß")])))
        `shouldBe`
        "<img id=\"äöüß\">"

    it "computes its result lazily (String)" $ do

      renderString (errorWithoutStackTrace "not lazy" :: 'Img > ())
        `shouldBe`
        "<img>"

      take 5 (renderString (div_ (errorWithoutStackTrace "not lazy" :: String)))
        `shouldBe`
        "<div>"

      take 5 (renderString (errorWithoutStackTrace "not lazy" :: 'Div > String))
        `shouldBe`
        "<div>"

      take 9 (renderString (img_A [("id", errorWithoutStackTrace "not lazy")]))
        `shouldBe`
        "<img id=\""

      take 12 (renderString (div_ "a" # (errorWithoutStackTrace "not lazy" :: String)))
        `shouldBe`
        "<div>a</div>"

      take 17 (renderString (div_ "a" # [img_ # (errorWithoutStackTrace "not lazy" :: String)]))
        `shouldBe`
        "<div>a</div><img>"

    it "computes its result lazily (Text)" $ do

      T.unpack (renderText (errorWithoutStackTrace "not lazy" :: 'Img > ()))
        `shouldBe`
        "<img>"

      take 5 (T.unpack (renderText (div_ (errorWithoutStackTrace "not lazy" :: String))))
        `shouldBe`
        "<div>"

      take 5 (T.unpack (renderText (errorWithoutStackTrace "not lazy" :: 'Div > String)))
        `shouldBe`
        "<div>"

      -- take 9 (T.unpack (renderText (img_A [("id", errorWithoutStackTrace "not lazy")])))
      --   `shouldBe`
      --   "<img id=\""

      take 12 (T.unpack (renderText (div_ "a" # (errorWithoutStackTrace "not lazy" :: String))))
        `shouldBe`
        "<div>a</div>"

      take 17 (T.unpack (renderText (div_ "a" # [img_ # (errorWithoutStackTrace "not lazy" :: String)])))
        `shouldBe`
        "<div>a</div><img>"

    it "computes its result lazily (ByteString)" $ do

      T.unpack (decodeUtf8 (renderByteString (errorWithoutStackTrace "not lazy" :: 'Img > ())))
        `shouldBe`
        "<img>"

      take 5 (T.unpack (decodeUtf8 (renderByteString (div_ (errorWithoutStackTrace "not lazy" :: String)))))
        `shouldBe`
        "<div>"

      take 5 (T.unpack (decodeUtf8 (renderByteString (errorWithoutStackTrace "not lazy" :: 'Div > String))))
        `shouldBe`
        "<div>"

      -- take 9 (T.unpack (renderText (img_A [("id", errorWithoutStackTrace "not lazy")])))
      --   `shouldBe`
      --   "<img id=\""

      take 12 (T.unpack (decodeUtf8 (renderByteString (div_ "a" # (errorWithoutStackTrace "not lazy" :: String)))))
        `shouldBe`
        "<div>a</div>"

      take 17 (T.unpack (decodeUtf8 (renderByteString (div_ "a" # [img_ # (errorWithoutStackTrace "not lazy" :: String)]))))
        `shouldBe`
        "<div>a</div><img>"
