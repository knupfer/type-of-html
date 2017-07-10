{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Html

main :: IO ()
main = putStrLn $ render (bigPage "TEST")

bigPage :: ('Title ?> a) =>
         a
         -> 'Html
            > (('Head
                > (('Meta > ())
                   # (('Title > a)
                      # (('Script > ()) # (('Link > ()) # ('Style > ()))))))
               # ('Body
                  > (('H1 > (('Img > ()) # ('Strong > ())))
                     # (('Div > (('Div > ('Img > ())) # ('Div > ('Img > ()))))
                        # ('Div
                           > ('Form
                              > ('Fieldset
                                 > (('Div
                                     > ('Div
                                        > (('Div
                                            > (('Label > ())
                                               # (('Select
                                                   > (('Option > ())
                                                      # ('Option > ())))
                                                  # ('Div > ()))))
                                           # ('I > ()))))
                                    # ('Button > ('I > ()))))))))))
bigPage x =
  html_
    ( head_
      ( meta_ ()
      # title_ x
      # script_ ()
      # link_ ()
      # style_ ()
      )
    # body_
      ( h1_
        ( img_ ()
        # strong_ ()
        )
      # div_
        ( div_ (img_ ())
        # div_ (img_ ())
        )
      # div_
        ( form_
          ( fieldset_
            ( div_
              ( div_
                ( div_
                  ( label_ ()
                  # select_
                    ( option_ ()
                    # option_ ()
                    )
                  # div_ ()
                  )
                # i_ ()
                )
              )
            # button_ (i_ ())
            )
          )
        )
      )
    )
