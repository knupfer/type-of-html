{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Medium where

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
  = I :@ AccesskeyA:="a"
    :> I :@ ClassA:="b"
       :> I :@ ContenteditableA:="c"
          :> I :@ TranslateA:="d"
             :> I :@ DirA:="e"
                :> I :@ DraggableA:="f"
                   :> I :@ HiddenA
                      :> I :@ IdA:="h"
                         :> I :@ ItempropA:="i"
                            :> I :@ LangA:="j"
                               :> I :@ SpellcheckA:="k"
                                  :> I :@ StyleA:="l"
                                     :> I :@ TitleA:=x :> "m"

attrLong x =
  I :@ ( AccesskeyA       := "a"
       # ClassA           := "b"
       # ContenteditableA := "c"
       # TranslateA       := "d"
       # DirA             := "e"
       # DraggableA       := "f"
       # HiddenA
       # IdA              := "h"
       # ItempropA        := "i"
       # LangA            := "j"
       # SpellcheckA      := "k"
       # StyleA           := "l"
       # TitleA           := x
       ) :> "m"

pageA x = Html :> Body
  :> ( H1 :@ IdA:="a"
     :> ( Img
        # Strong :@ ClassA:="b" :> (0 :: Int)
        )
     # Div :> Div :@ IdA:="c" :> (1 :: Int)
     # Div :> Form :@ ClassA:="d" :> Fieldset
     :> ( Div :@ IdA:="e"
        :> ( Div
           :> ( Label :@ ClassA:="f" :> "h"
              # Select
              :> ( Option :@ IdA:="i" :> "j"
                 # Option :> "k"
                 )
              # Div :@ ClassA:="l" :> "m"
              )
           # I :> x
           )
        # Button :@ IdA:="n" :> I :> "o"
        )
     )

