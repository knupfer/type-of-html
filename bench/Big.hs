{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Big where

import Html
import Medium (attrShort, attrLong, table)

page x =
  Html :>
    ( Body :>
      ( H1 :@ (IdA := "a") :>
        ( Img
        # Strong :@ (ClassA := "b") :> (0 :: Int)
        )
      # Div :>
        ( Div :@ (IdA := "c") :> (1 :: Int)
        )
      # attrShort ""
      # attrLong ""
      # table (3,3)
      # Div :>
        ( Form :@ (ClassA := "d") :>
          ( Fieldset :>
            ( Div :@ (IdA := "e") :>
              ( Div :>
                ( Label :@ (ClassA := "f") :> "g"
                # Select :>
                  ( Option :@ (IdA := "h") :> "i"
                  # Option :> "j"
                  )
                # Div :@ (ClassA := "k") :> "l"
                )
              # I :> x
              )
            # Button :@ (IdA := "m") :> (I :> "n")
            )
          )
        )
      # Div :>
        ( Form :@ (ClassA := "o") :>
          ( Fieldset :>
            ( Div :@ (IdA := "p") :>
              ( Div :>
                ( Label :@ (ClassA := "q") :> "r"
                # Select :>
                  ( Option :@ (IdA := "s") :> "4"
                  # Option :> "u"
                  )
                # Div :@ (ClassA := "v") :> "w"
                )
              # I :> x
              )
            # Button :@ (IdA := "x") :> (I :> "y")
            )
          )
        )
      # Div :>
        ( Form :@ (ClassA := "z") :>
          ( Fieldset :>
            ( Div :@ (IdA := "A") :>
              ( Div :>
                ( Label :@ (ClassA := "B") :> "C"
                # Select :>
                  ( Option :@ (IdA := "D") :> "E"
                  # Option :> "F"
                  )
                # Div :@ (ClassA := "G") :> "H"
                )
              # I :> x
              )
            # Button :@ (IdA := "I") :> (I :> "J")
            )
          )
        )
      # Div :>
        ( Form :@ (ClassA := "K") :>
          ( Fieldset :>
            ( Div :@ (IdA := "L") :>
              ( Div :>
                ( Label :@ (ClassA := "M") :> "N"
                # Select :>
                  ( Option :@ (IdA := "O") :> "P"
                  # Option :> "Q"
                  )
                # Div :@ (ClassA := "R") :> "S"
                )
              # I :> x
              )
            # Button :@ (IdA := "T") :> (I :> "U")
            )
          )
        )
      )
    )
