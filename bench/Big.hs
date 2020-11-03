{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Big where

import Html
import Medium (attrShort, attrLong, table)

page x =
  html_
    ( body_
      ( h1_A (IdA := "a")
        ( img_
        # strong_A (ClassA := "b") (0 :: Int)
        )
      # div_
        ( div_A (IdA := "c") (1 :: Int)
        )
      # attrShort ""
      # attrLong ""
      # table (3,3)
      # div_
        ( form_A (ClassA := "d")
          ( fieldset_
            ( div_A (IdA := "e")
              ( div_
                ( label_A (ClassA := "f") "g"
                # select_
                  ( option_A (IdA := "h") "i"
                  # option_ "j"
                  )
                # div_A (ClassA := "k") "l"
                )
              # i_ x
              )
            # button_A (IdA := "m") (i_ "n")
            )
          )
        )
      # div_
        ( form_A (ClassA := "o")
          ( fieldset_
            ( div_A (IdA := "p")
              ( div_
                ( label_A (ClassA := "q") "r"
                # select_
                  ( option_A (IdA := "s") "4"
                  # option_ "u"
                  )
                # div_A (ClassA := "v") "w"
                )
              # i_ x
              )
            # button_A (IdA := "x") (i_ "y")
            )
          )
        )
      # div_
        ( form_A (ClassA := "z")
          ( fieldset_
            ( div_A (IdA := "A")
              ( div_
                ( label_A (ClassA := "B") "C"
                # select_
                  ( option_A (IdA := "D") "E"
                  # option_ "F"
                  )
                # div_A (ClassA := "G") "H"
                )
              # i_ x
              )
            # button_A (IdA := "I") (i_ "J")
            )
          )
        )
      # div_
        ( form_A (ClassA := "K")
          ( fieldset_
            ( div_A (IdA := "L")
              ( div_
                ( label_A (ClassA := "M") "N"
                # select_
                  ( option_A (IdA := "O") "P"
                  # option_ "Q"
                  )
                # div_A (ClassA := "R") "S"
                )
              # i_ x
              )
            # button_A (IdA := "T") (i_ "U")
            )
          )
        )
      )
    )
