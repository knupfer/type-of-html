{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html
import qualified Html.Attribute as A

import Data.Proxy
import Test.Hspec
import Test.QuickCheck

import Data.Text.Lazy.Encoding

import qualified Data.Text.Lazy as T

newtype Escaped = Escaped String deriving Show

instance Arbitrary Escaped where
  arbitrary = Escaped <$> arbitrary `suchThat` (\x -> all (`notElem` x) "<>&\"'")

main :: IO ()
main = hspec spec

spec :: Spec
spec = let allT a b c = b (renderString a, T.unpack $ renderText a, T.unpack . decodeUtf8 $ renderByteString a)
                          (c, c, c)

       in parallel $ do

  describe "render" $ do

    it "is id on strings without escaping chars" $ do

      property $ \(Escaped x) -> allT x (==) x

    it "handles single elements" $ do

      property $ \(Escaped x) -> allT (div_ x) (==) ("<div>" ++ x ++ "</div>")

    it "handles nested elements" $ do

      property $ \(Escaped x) -> allT (div_ (div_ x)) (==)
        ("<div><div>" ++ x ++ "</div></div>")

    it "handles parallel elements" $ do

      property $ \(Escaped x) (Escaped y) -> allT (div_ x # div_ y)
        (==)
        ("<div>" ++ x ++ "</div><div>" ++ y ++ "</div>")

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

    it "handles trailing text" $ do

      property $ \(Escaped x) (Escaped y) -> allT (td_ x # y)
        (==)
        ("<td>" ++ x ++ "</td>" ++ y)

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

      allT [div_ "a" # i_ "b"]
        shouldBe
        "<div>a</div><i>b</i>"

    it "handles nested lists" $ do

      allT (div_ [div_ [div_ (4 :: Int)]])
        shouldBe
        "<div><div><div>4</div></div></div>"

    it "handles utf8 correctly" $ do

      allT (div_ "a ä € 𝄞")
        shouldBe
        "<div>a ä € 𝄞</div>"

      allT (img_A (A.id_ "a ä € 𝄞"))
        shouldBe
        "<img id=\"a ä € 𝄞\">"
