{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}

module Medium where

import Html

import Test.QuickCheck
import Criterion.Main

import qualified Html.Attribute as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

medium :: Benchmark
medium = bgroup "Medium"
  [ bench "helloWorld"          $ nf (renderByteString . helloWorld) "medium"
  , bench "helloWorld'"         $ nf (renderByteString . helloWorld') "medium"
  , bench "randomString"        $ nfIO randomString
  , bench "randomStringRaw"     $ nfIO randomStringRaw
  , bench "randomStrictText"    $ nfIO randomStrictText
  , bench "randomStrictTextRaw" $ nfIO randomStrictTextRaw
  , bench "randomLazyText"      $ nfIO randomLazyText
  , bench "randomLazyTextRaw"   $ nfIO randomLazyTextRaw
  , bench "table"               $ nf (renderByteString . table) (2,2)
  , bench "table'"              $ nf (renderByteString . table') (2,2)
  , bench "page"                $ nf (renderByteString . page) "medium"
  , bench "page'"               $ nf (renderByteString . page') "medium"
  , bench "pageA"               $ nf (renderByteString . pageA) "medium"
  , bench "pageA'"              $ nf (renderByteString . pageA') "medium"
  , bench "attrShort"           $ nf (renderByteString . attrShort) "medium"
  , bench "attrShort'"          $ nf (renderByteString . attrShort') "medium"
  , bench "attrShort''"         $ nf (renderByteString . attrShort'') "medium"
  , bench "attrLong"            $ nf (renderByteString . attrLong) "medium"
  , bench "attrLong'"           $ nf (renderByteString . attrLong') "medium"
  , bench "attrLong''"          $ nf (renderByteString . attrLong'') "medium"
  ]

randomString = do
  a <- generate (resize 5 arbitrary) :: IO String
  return . renderByteString $ div_ a

randomStringRaw = do
  a <- generate (resize 5 arbitrary) :: IO String
  return . renderByteString $ div_ (Raw a)

randomStrictText = do
  a <- T.pack <$> generate (resize 5 arbitrary) :: IO T.Text
  return . renderByteString $ div_ a

randomStrictTextRaw = do
  a <- T.pack <$> generate (resize 5 arbitrary) :: IO T.Text
  return . renderByteString $ div_ (Raw a)

randomLazyText = do
  a <- LT.pack <$> generate (resize 5 arbitrary) :: IO LT.Text
  return . renderByteString $ div_ a

randomLazyTextRaw = do
  a <- LT.pack <$> generate (resize 5 arbitrary) :: IO LT.Text
  return . renderByteString $ div_ (Raw a)

helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ "Hello World!"
      )
    )

helloWorld' = \x ->
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ "Hello World!"
      )
    )

table (n, m) = table_ . replicate n . tr_ $ map td_ [(1::Int)..m]

table' = \(n,m) -> table_ . replicate n . tr_ $ map td_ [(1::Int)..m]

page x =
  html_
    ( body_
      ( h1_
        ( img_
        # strong_ (0 :: Int)
        )
      # div_
        ( div_ (1 :: Int)
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( label_ "a"
                # select_
                  ( option_ "b"
                  # option_ "c"
                  )
                # div_ "d"
                )
              # i_ x
              )
            # button_ (i_ "e")
            )
          )
        )
      )
    )

page' = \x ->
  html_
    ( body_
      ( h1_
        ( img_
        # strong_ (0 :: Int)
        )
      # div_
        ( div_ (1 :: Int)
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( label_ "a"
                # select_
                  ( option_ "b"
                  # option_ "c"
                  )
                # div_ "d"
                )
              # i_ x
              )
            # button_ (i_ "e")
            )
          )
        )
      )
    )


attrShort x
  = i_A (A.accesskey_       "a")
  . i_A (A.class_           "b")
  . i_A (A.contenteditable_ "c")
  . i_A (A.contextmenu_     "d")
  . i_A (A.dir_             "e")
  . i_A (A.draggable_       "f")
  . i_A (A.hidden_             )
  . i_A (A.id_              "h")
  . i_A (A.itemprop_        "i")
  . i_A (A.lang_            "j")
  . i_A (A.spellcheck_      "k")
  . i_A (A.style_           "l")
  . i_A (A.title_           "m")
  $ x

attrShort'
  = i_A (A.accesskey_       "a")
  . i_A (A.class_           "b")
  . i_A (A.contenteditable_ "c")
  . i_A (A.contextmenu_     "d")
  . i_A (A.dir_             "e")
  . i_A (A.draggable_       "f")
  . i_A (A.hidden_             )
  . i_A (A.id_              "h")
  . i_A (A.itemprop_        "i")
  . i_A (A.lang_            "j")
  . i_A (A.spellcheck_      "k")
  . i_A (A.style_           "l")
  . i_A (A.title_           "m")

attrShort''
  = \x ->
    i_A (A.accesskey_       "a")
  . i_A (A.class_           "b")
  . i_A (A.contenteditable_ "c")
  . i_A (A.contextmenu_     "d")
  . i_A (A.dir_             "e")
  . i_A (A.draggable_       "f")
  . i_A (A.hidden_             )
  . i_A (A.id_              "h")
  . i_A (A.itemprop_        "i")
  . i_A (A.lang_            "j")
  . i_A (A.spellcheck_      "k")
  . i_A (A.style_           "l")
  . i_A (A.title_           "m")
  $ x

attrLong x =
  i_A ( A.accesskey_       "a"
      # A.class_           "b"
      # A.contenteditable_ "c"
      # A.contextmenu_     "d"
      # A.dir_             "e"
      # A.draggable_       "f"
      # A.hidden_
      # A.id_              "h"
      # A.itemprop_        "i"
      # A.lang_            "j"
      # A.spellcheck_      "k"
      # A.style_           "l"
      # A.title_           "n"
      ) x

attrLong' =
  i_A ( A.accesskey_       "a"
      # A.class_           "b"
      # A.contenteditable_ "c"
      # A.contextmenu_     "d"
      # A.dir_             "e"
      # A.draggable_       "f"
      # A.hidden_
      # A.id_              "h"
      # A.itemprop_        "i"
      # A.lang_            "j"
      # A.spellcheck_      "k"
      # A.style_           "l"
      # A.title_           "m"
      )

attrLong'' = \x ->
  i_A ( A.accesskey_       "a"
      # A.class_           "b"
      # A.contenteditable_ "c"
      # A.contextmenu_     "d"
      # A.dir_             "e"
      # A.draggable_       "f"
      # A.hidden_
      # A.id_              "h"
      # A.itemprop_        "i"
      # A.lang_            "j"
      # A.spellcheck_      "k"
      # A.style_           "l"
      # A.title_           "m"
      ) x

pageA x =
  html_
    ( body_
      ( h1_A (A.id_ "a")
        ( img_
        # strong_A (A.class_ "b") (0 :: Int)
        )
      # div_
        ( div_A (A.id_ "c") (1 :: Int)
        )
      # div_
        ( form_A (A.class_ "d")
          ( fieldset_
            ( div_A (A.id_ "e")
              ( div_
                ( label_A (A.class_ "f") "h"
                # select_
                  ( option_A (A.id_ "i") "j"
                  # option_ "k"
                  )
                # div_A (A.class_ "l") "m"
                )
              # i_ x
              )
            # button_A (A.id_ "n") (i_ "o")
            )
          )
        )
      )
    )

pageA' = \x ->
  html_
    ( body_
      ( h1_A (A.id_ "a")
        ( img_
        # strong_A (A.class_ "b") (0 :: Int)
        )
      # div_
        ( div_A (A.id_ "c") (1 :: Int)
        )
      # div_
        ( form_A (A.class_ "d")
          ( fieldset_
            ( div_A (A.id_ "e")
              ( div_
                ( label_A (A.class_ "f") "g"
                # select_
                  ( option_A (A.id_ "h") "i"
                  # option_ "j"
                  )
                # div_A (A.class_ "k") "l"
                )
              # i_ x
              )
            # button_A (A.id_ "m") (i_ "n")
            )
          )
        )
      )
    )
