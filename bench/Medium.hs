{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Medium where

import Html

helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ "Hello World!"
      )
    )

table (n, m) = table_ . replicate n . tr_ $ map td_ [(1::Int)..m]

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

attrShort x =
  i_A (AccesskeyA := "a")
  ( i_A (ClassA := "b")
    ( i_A (ContenteditableA := "c")
      ( i_A (TranslateA := "d")
        ( i_A (DirA := "e")
          ( i_A (DraggableA := "f")
            ( i_A HiddenA
              ( i_A (IdA := "h")
                ( i_A (ItempropA := "i")
                  ( i_A (LangA := "j")
                    ( i_A (SpellcheckA := "k")
                      ( i_A (StyleA := "l")
                        ( i_A (TitleA := x) "m"))))))))))))

attrLong x =
  i_A ( AccesskeyA :=       "a"
      # ClassA :=           "b"
      # ContenteditableA := "c"
      # TranslateA :=     "d"
      # DirA :=             "e"
      # DraggableA :=       "f"
      # HiddenA
      # IdA :=              "h"
      # ItempropA :=        "i"
      # LangA :=            "j"
      # SpellcheckA :=      "k"
      # StyleA :=           "l"
      # TitleA :=           x
      ) "m"

pageA x =
  html_
    ( body_
      ( h1_A (IdA := "a")
        ( img_
        # strong_A (ClassA := "b") (0 :: Int)
        )
      # div_
        ( div_A (IdA := "c") (1 :: Int)
        )
      # div_
        ( form_A (ClassA := "d")
          ( fieldset_
            ( div_A (IdA := "e")
              ( div_
                ( label_A (ClassA := "f") "h"
                # select_
                  ( option_A (IdA := "i") "j"
                  # option_ "k"
                  )
                # div_A (ClassA := "l") "m"
                )
              # i_ x
              )
            # button_A (IdA := "n") (i_ "o")
            )
          )
        )
      )
    )
