{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html
import Test.Hspec

import Data.Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

  describe "render" $ do

    it "is id on empty string" $ do

      render ""
        `shouldBe`
        ""

    it "handles single elements" $ do

      render (div_ "a")
        `shouldBe`
        "<div>a</div>"

    it "handles nested elements" $ do

      render (div_ (div_ "a"))
        `shouldBe`
        "<div><div>a</div></div>"

    it "handles parallel elements" $ do

      render (div_ "a" # div_ "b")
        `shouldBe`
        "<div>a</div><div>b</div>"

    it "doesn't use closing tags for empty elements" $ do

      render area_
        `shouldBe`
        "<area>"

      render base_
        `shouldBe`
        "<base>"

      render br_
        `shouldBe`
        "<br>"

      render col_
        `shouldBe`
        "<col>"

      render embed_
        `shouldBe`
        "<embed>"

      render hr_
        `shouldBe`
        "<hr>"

      render iframe_
        `shouldBe`
        "<iframe>"

      render img_
        `shouldBe`
        "<img>"

      render link_
        `shouldBe`
        "<link>"

      render menuitem_
        `shouldBe`
        "<menuitem>"

      render meta_
        `shouldBe`
        "<meta>"

      render param_
        `shouldBe`
        "<param>"

      render source_
        `shouldBe`
        "<source>"

      render track_
        `shouldBe`
        "<track>"

      render wbr_
        `shouldBe`
        "<wbr>"

    it "avoids optional closing tags" $ do

      -- The closing tag at the end is because we can't know what
      -- element will follow.

      render (td_ () # td_ ())
        `shouldBe`
        "<td><td></td>"

      render (tr_ $ td_ ())
        `shouldBe`
        "<tr><td></tr>"

      render (table_ . tr_ $ td_ ())
        `shouldBe`
        "<table><tr><td></table>"

    it "handles trailing text" $ do

      render (td_ "a" # "b")
        `shouldBe`
        "<td>a</td>b"

    it "handles a single compile time text" $ do

      render (Proxy :: Proxy "a")
        `shouldBe`
        "a"

    it "handles trailing compile time text" $ do

      render (div_ "a" # (Proxy :: Proxy "b"))
        `shouldBe`
        "<div>a</div>b"

    it "handles nested compile time text" $ do

      render (div_ (Proxy :: Proxy "a"))
        `shouldBe`
        "<div>a</div>"

    it "handles a list of compile time text" $ do

      render (replicate 5 (Proxy :: Proxy "a"))
        `shouldBe`
        "aaaaa"

    it "handles an empty list of compile time text" $ do

      render (replicate 0 (Proxy :: Proxy "a"))
        `shouldBe`
        ""

    it "handles an empty list" $ do

      render (tail [td_ "a"])
        `shouldBe`
        ""

    it "handles a list with a single element" $ do

      render [td_ "a"]
        `shouldBe`
        "<td>a</td>"

    it "handles tags in a list with parallel elements" $ do

      render [td_ "a" # td_ "b"]
        `shouldBe`
        "<td>a<td>b</td>"

    it "handles tags in a list with parallel elements and a following tag" $ do

      render ([td_ "a" # td_ "b"] # td_ "c")
        `shouldBe`
        "<td>a<td>b<td>c</td>"

      render ([td_ "a" # td_ "b"] # div_ "c")
        `shouldBe`
        "<td>a<td>b</td><div>c</div>"

      render ([div_ "a" # td_ "b"] # td_ "c")
        `shouldBe`
        "<div>a</div><td>b</td><td>c</td>"
