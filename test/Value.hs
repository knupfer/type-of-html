{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html

import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Custom

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

  describe "render" $ do

    it "is id on strings without escaping" $ do

      property $ \x ->
        renderString (Raw x)
        ===
        x

    it "handles single elements" $ do

      property $ \x ->
        renderString (div_ (Raw x))
        ===
        "<div>" ++ x ++ "</div>"

    it "handles nested elements" $ do

      property $ \x ->
        renderString (div_ (div_ (Raw x)))
        ===
        "<div><div>" ++ x ++ "</div></div>"

    it "handles parallel elements" $ do

      property $ \x y ->
        renderString (div_ (Raw x) # div_ (Raw y))
        ===
        "<div>" ++ x ++ "</div><div>" ++ y ++ "</div>"

    it "doesn't use closing tags for empty elements" $ do

      renderString area_
        `shouldBe`
        "<area>"

      renderString base_
       `shouldBe`
        "<base>"

      renderString br_
       `shouldBe`
        "<br>"

      renderString col_
       `shouldBe`
        "<col>"

      renderString embed_
       `shouldBe`
        "<embed>"

      renderString hr_
       `shouldBe`
        "<hr>"

      renderString iframe_
       `shouldBe`
        "<iframe>"

      renderString img_
       `shouldBe`
        "<img>"

      renderString link_
       `shouldBe`
        "<link>"

      renderString meta_
       `shouldBe`
        "<meta>"

      renderString param_
       `shouldBe`
        "<param>"

      renderString source_
       `shouldBe`
        "<source>"

      renderString track_
       `shouldBe`
        "<track>"

      renderString wbr_
       `shouldBe`
        "<wbr>"

    it "handles trailing text" $ do

      property $ \x y ->
        renderString (td_ (Raw x) # (Raw y))
        ===
        "<td>" ++ x ++ "</td>" ++ y

    it "handles a single compile time text" $ do

      renderString (Proxy :: Proxy "a")
       `shouldBe`
        "a"

    it "handles trailing compile time text" $ do

      renderString (div_ "a" # (Proxy :: Proxy "b"))
       `shouldBe`
        "<div>a</div>b"

    it "handles nested compile time text" $ do

      renderString (div_ (Proxy :: Proxy "a"))
       `shouldBe`
        "<div>a</div>"

    it "handles an empty list" $ do

      renderString (tail [td_ "a"])
       `shouldBe`
        ""

    it "handles a list with a single element" $ do

      renderString [td_ "a"]
       `shouldBe`
        "<td>a</td>"

    it "handles tags in a list with parallel elements" $ do

      renderString [div_ "a" # i_ "b"]
       `shouldBe`
        "<div>a</div><i>b</i>"

    it "handles nested lists" $ do

      renderString (div_ [div_ [div_ (4 :: Int)]])
       `shouldBe`
        "<div><div><div>4</div></div></div>"

    it "handles utf8 correctly" $ do

      renderString (div_ "a ä € 𝄞")
       `shouldBe`
        "<div>a ä € 𝄞</div>"

      renderString (img_A (IdA := "a ä € 𝄞"))
       `shouldBe`
        "<img id=\"a ä € 𝄞\">"

    it "handles Chars" $ do

      property $ \x ->
        renderString (div_ [x :: Char])
        ===
        renderString (div_ x)

    it "handles maybes" $ do

      renderString (div_ (Just (div_ "a")))
       `shouldBe`
        "<div><div>a</div></div>"

      renderString (Just (42 :: Int))
       `shouldBe`
        "42"

      renderString (div_A (Just (IdA := "a")) "b")
       `shouldBe`
        "<div id=\"a\">b</div>"

      renderString (div_ (if True then Nothing else Just (div_ "a")))
       `shouldBe`
        "<div></div>"

    it "handles eithers" $ do

      renderString (div_ (if True then Left (div_ "a") else Right "b"))
       `shouldBe`
        "<div><div>a</div></div>"

      renderString (div_A (if True then Right (IdA := "a") else Left (ClassA := "a")) "b")
       `shouldBe`
        "<div id=\"a\">b</div>"

    it "handles attributes" $ do

      renderString (div_A (IdA := "a") "b" # "c")
       `shouldBe`
        "<div id=\"a\">b</div>c"

      renderString (div_A (IdA := ()) "a")
       `shouldBe`
        "<div id>a</div>"

      renderString (div_A HiddenA "a")
       `shouldBe`
        "<div hidden>a</div>"

      renderString (div_A HiddenA ())
       `shouldBe`
        "<div hidden></div>"

      renderString (div_A HiddenA () # "a")
       `shouldBe`
        "<div hidden></div>a"

      renderString (div_A HiddenA () # img_)
       `shouldBe`
        "<div hidden></div><img>"

    it "handles custom attributes" $ do

      renderString (div_A (hxPost_ "x") "y")
        `shouldBe`
        "<div hx-post=\"x\">y</div>"

    it "handles Ints" $ do

      property $ \x ->
        renderString (div_ (x :: Int))
        ===
        renderString (div_ (show x))

    it "handles complex compile time documents" $ do

      renderString (div_ () # i_ ())
       `shouldBe`
        "<div></div><i></i>"

      renderString (div_ () # "a")
       `shouldBe`
        "<div></div>a"

      renderString ("a" # i_ ())
       `shouldBe`
        "a<i></i>"

      renderString (div_ () # i_ (Proxy @"a"))
       `shouldBe`
        "<div></div><i>a</i>"

      renderString (div_ (Proxy @"a") # i_ ())
       `shouldBe`
        "<div>a</div><i></i>"

      renderString (Proxy @"1" # "2")
       `shouldBe`
        "12"

      renderString ("1" # Proxy @"2")
       `shouldBe`
        "12"

      renderString (div_ () # td_ (Proxy @"1" # "2" # div_ () # i_A (IdA := (Proxy @"3")) "4"))
       `shouldBe`
        "<div></div><td>12<div></div><i id=\"3\">4</i></td>"

    it "handles list of convertibles" $ do

      renderString (div_ [1..5 :: Int])
       `shouldBe`
        "<div>12345</div>"

      renderString [1..5 :: Int]
       `shouldBe`
        "12345"

      renderString (div_ ["abc"])
       `shouldBe`
        "<div>abc</div>"
