{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Big where

import Html
import Medium (attrShort, attrLong, table)

import qualified Html.Attribute as A

page x =
  html_
    ( body_
      ( h1_A (A.id_ "a")
        ( img_
        # strong_A (A.class_ "b") (0 :: Int)
        )
      # div_
        ( div_A (A.id_ "c") (1 :: Int)
        )
      # attrShort ""
      # attrLong ""
      # table (3,3)
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
      # div_
        ( form_A (A.class_ "o")
          ( fieldset_
            ( div_A (A.id_ "p")
              ( div_
                ( label_A (A.class_ "q") "r"
                # select_
                  ( option_A (A.id_ "s") "4"
                  # option_ "u"
                  )
                # div_A (A.class_ "v") "w"
                )
              # i_ x
              )
            # button_A (A.id_ "x") (i_ "y")
            )
          )
        )
      # div_
        ( form_A (A.class_ "z")
          ( fieldset_
            ( div_A (A.id_ "A")
              ( div_
                ( label_A (A.class_ "B") "C"
                # select_
                  ( option_A (A.id_ "D") "E"
                  # option_ "F"
                  )
                # div_A (A.class_ "G") "H"
                )
              # i_ x
              )
            # button_A (A.id_ "I") (i_ "J")
            )
          )
        )
      # div_
        ( form_A (A.class_ "K")
          ( fieldset_
            ( div_A (A.id_ "L")
              ( div_
                ( label_A (A.class_ "M") "N"
                # select_
                  ( option_A (A.id_ "O") "P"
                  # option_ "Q"
                  )
                # div_A (A.class_ "R") "S"
                )
              # i_ x
              )
            # button_A (A.id_ "T") (i_ "U")
            )
          )
        )
      )
    )

page' = \x ->
  html_
    ( body_
      ( h1_A (A.id_ "a")
        ( img_
        # strong_A (A.class_ "b") (0 :: Int)
        )
      # div_
        ( div_A (A.id_ "c") (1 :: Int)
        )
      # attrShort ""
      # attrLong ""
      # table (3,3)
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
      # div_
        ( form_A (A.class_ "o")
          ( fieldset_
            ( div_A (A.id_ "p")
              ( div_
                ( label_A (A.class_ "q") "r"
                # select_
                  ( option_A (A.id_ "s") "4"
                  # option_ "u"
                  )
                # div_A (A.class_ "v") "w"
                )
              # i_ x
              )
            # button_A (A.id_ "x") (i_ "y")
            )
          )
        )
      # div_
        ( form_A (A.class_ "z")
          ( fieldset_
            ( div_A (A.id_ "A")
              ( div_
                ( label_A (A.class_ "B") "C"
                # select_
                  ( option_A (A.id_ "D") "E"
                  # option_ "F"
                  )
                # div_A (A.class_ "G") "H"
                )
              # i_ x
              )
            # button_A (A.id_ "I") (i_ "J")
            )
          )
        )
      # div_
        ( form_A (A.class_ "K")
          ( fieldset_
            ( div_A (A.id_ "L")
              ( div_
                ( label_A (A.class_ "M") "N"
                # select_
                  ( option_A (A.id_ "O") "P"
                  # option_ "Q"
                  )
                # div_A (A.class_ "R") "S"
                )
              # i_ x
              )
            # button_A (A.id_ "T") (i_ "U")
            )
          )
        )
      )
    )
