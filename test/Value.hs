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
        renderString (Div :> Raw x)
        ===
        "<div>" ++ x ++ "</div>"

    it "handles nested elements" $ do

      property $ \x ->
        renderString (Div :> Div :> Raw x)
        ===
        "<div><div>" ++ x ++ "</div></div>"

    it "handles parallel elements" $ do

      property $ \x y ->
        renderString (Div :> Raw x # Div :> Raw y)
        ===
        "<div>" ++ x ++ "</div><div>" ++ y ++ "</div>"

    it "doesn't use closing tags for empty elements" $ do

      renderString Area
        `shouldBe`
        "<area>"

      renderString Base
       `shouldBe`
        "<base>"

      renderString Br
       `shouldBe`
        "<br>"

      renderString Col
       `shouldBe`
        "<col>"

      renderString Embed
       `shouldBe`
        "<embed>"

      renderString Hr
       `shouldBe`
        "<hr>"

      renderString Iframe
       `shouldBe`
        "<iframe>"

      renderString Img
       `shouldBe`
        "<img>"

      renderString Link
       `shouldBe`
        "<link>"

      renderString Meta
       `shouldBe`
        "<meta>"

      renderString Param
       `shouldBe`
        "<param>"

      renderString Source
       `shouldBe`
        "<source>"

      renderString Track
       `shouldBe`
        "<track>"

      renderString Wbr
       `shouldBe`
        "<wbr>"

    it "handles trailing text" $ do

      property $ \x y ->
        renderString (Td :> (Raw x) # (Raw y))
        ===
        "<td>" ++ x ++ "</td>" ++ y

    it "handles a single compile time text" $ do

      renderString (Proxy :: Proxy "a")
       `shouldBe`
        "a"

    it "handles trailing compile time text" $ do

      renderString (Div :> "a" # (Proxy :: Proxy "b"))
       `shouldBe`
        "<div>a</div>b"

    it "handles nested compile time text" $ do

      renderString (Div :> (Proxy :: Proxy "a"))
       `shouldBe`
        "<div>a</div>"

    it "handles an empty list" $ do

      renderString (tail [Td :> "a"])
       `shouldBe`
        ""

    it "handles a list with a single element" $ do

      renderString [Td :> "a"]
       `shouldBe`
        "<td>a</td>"

    it "handles tags in a list with parallel elements" $ do

      renderString [Div :> "a" # I :> "b"]
       `shouldBe`
        "<div>a</div><i>b</i>"

    it "handles nested lists" $ do

      renderString (Div :> [Div :> [Div :> (4 :: Int)]])
       `shouldBe`
        "<div><div><div>4</div></div></div>"

    it "handles utf8 correctly" $ do

      renderString (Div :> "a ä € 𝄞")
       `shouldBe`
        "<div>a ä € 𝄞</div>"

      renderString (Img :@ (IdA "a ä € 𝄞"))
       `shouldBe`
        "<img id=\"a ä € 𝄞\">"

    it "handles Chars" $ do

      property $ \x ->
        renderString (Div :> [x :: Char])
        ===
        renderString (Div :> x)

    it "handles maybes" $ do

      renderString (Div :> (Just (Div :> "a")))
       `shouldBe`
        "<div><div>a</div></div>"

      renderString (Just (42 :: Int))
       `shouldBe`
        "42"

      renderString (Div :@ (Just (IdA "a")) :> "b")
       `shouldBe`
        "<div id=\"a\">b</div>"

      renderString (Div :> (if True then Nothing else Just (Div :> "a")))
       `shouldBe`
        "<div></div>"

    it "handles eithers" $ do

      renderString (Div :> (if True then Left (Div :> "a") else Right "b"))
       `shouldBe`
        "<div><div>a</div></div>"

      renderString (Div :@ (if True then Right (IdA "a") else Left (ClassA "a")) :> "b")
       `shouldBe`
        "<div id=\"a\">b</div>"

    it "handles attributes" $ do

      renderString (Div :@ (IdA "a") :> "b" # "c")
       `shouldBe`
        "<div id=\"a\">b</div>c"

      renderString (Div :@ (IdA ()) :> "a")
       `shouldBe`
        "<div id>a</div>"

      renderString (Div :@ HiddenA :> "a")
       `shouldBe`
        "<div hidden>a</div>"

      renderString (Div :@ HiddenA :> ())
       `shouldBe`
        "<div hidden></div>"

      renderString (Div :@ HiddenA :> () # "a")
       `shouldBe`
        "<div hidden></div>a"

      renderString (Div :@ HiddenA :> () # Img)
       `shouldBe`
        "<div hidden></div><img>"

    it "handles custom attributes" $ do

      renderString (Div :@ (HxPostA "x") :> "y")
        `shouldBe`
        "<div hx-post=\"x\">y</div>"

    it "handles Ints" $ do

      property $ \x ->
        renderString (Div :> (x :: Int))
        ===
        renderString (Div :> (show x))

    it "handles complex compile time documents" $ do

      renderString (Div :> () # I :> ())
       `shouldBe`
        "<div></div><i></i>"

      renderString (Div :> () # "a")
       `shouldBe`
        "<div></div>a"

      renderString ("a" # I :> ())
       `shouldBe`
        "a<i></i>"

      renderString (Div :> () # I :> (Proxy @"a"))
       `shouldBe`
        "<div></div><i>a</i>"

      renderString (Div :> (Proxy @"a") # I :> ())
       `shouldBe`
        "<div>a</div><i></i>"

      renderString (Proxy @"1" # "2")
       `shouldBe`
        "12"

      renderString ("1" # Proxy @"2")
       `shouldBe`
        "12"

      renderString (Div :> () # Td :> (Proxy @"1" # "2" # Div :> () # I :@ (IdA (Proxy @"3")) :> "4"))
       `shouldBe`
        "<div></div><td>12<div></div><i id=\"3\">4</i></td>"

    it "handles list of convertibles" $ do

      renderString (Div :> [1..5 :: Int])
       `shouldBe`
        "<div>12345</div>"

      renderString [1..5 :: Int]
       `shouldBe`
        "12345"

      renderString (Div :> ["abc"])
       `shouldBe`
        "<div>abc</div>"
