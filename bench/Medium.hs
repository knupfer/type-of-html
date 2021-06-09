{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}

module Medium where

import Prelude
import Html

helloWorld x
  = Html
  :> ( Head :> Title :> x
     # Body :> P :> "Hello World!"
     )

table (n, m) = (Table :>) . replicate n . (Tr :>) $ map (Td :>) [(1::Int)..m]

page x
  = Html
  :> ( Body
     :> ( H1
        :> ( Img
           # Strong :> (0 :: Int)
           )
        # Div :> Div :> (1 :: Int)
        # Div
        :> ( Form
           :> ( Fieldset
              :> ( Div
                 :> ( Div
                    :> ( Label :> "a"
                       # Select
                       :> ( Option :> "b"
                          # Option :> "c"
                          )
                       # Div :> "d"
                       )
                    # I :> x
                    )
                 # Button :> I :> "e"
                 )
              )
           )
        )
     )

attrShort x
  = I :@ AccesskeyA "a"
    :> I :@ ClassA "b"
       :> I :@ ContenteditableA "c"
          :> I :@ TranslateA "d"
             :> I :@ DirA "e"
                :> I :@ DraggableA "f"
                   :> I :@ HiddenA
                      :> I :@ IdA "h"
                         :> I :@ ItempropA "i"
                            :> I :@ LangA "j"
                               :> I :@ SpellcheckA "k"
                                  :> I :@ StyleA "l"
                                     :> I :@ TitleA x :> "m"

attrLong x =
  I :@ ( AccesskeyA         "a"
       # ClassA             "b"
       # ContenteditableA   "c"
       # TranslateA         "d"
       # DirA               "e"
       # DraggableA         "f"
       # HiddenA
       # IdA                "h"
       # ItempropA          "i"
       # LangA              "j"
       # SpellcheckA        "k"
       # StyleA             "l"
       # TitleA             x
       ) :> "m"

pageA x =
  Html :> do
    Body :> do
      H1 :@ IdA "a" :> do
        Img
        Strong :@ ClassA "b" :> (0 :: Int)
      Div :> do
        Div :@ IdA "c" :> (1 :: Int)
      Div :> do
        Form :@ ClassA "d" :> do
          Fieldset :> do
            Div :@ IdA "e" :> do
              Div :> do
                Label :@ ClassA "f" :> "a"
                Select :> do
                  Option :@ IdA "g" :> "b"
                  Option :> "c"
                Div :@ ClassA "h" :> "d"
              I :> x
            Button :@ IdA "i" :> do
              I :> "e"
  where
    (>>) = (#)

