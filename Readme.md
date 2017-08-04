# Type of html

`Type of html` is a library for generating html in a highly performant
and type safe manner.

Please read the documentation of the module:
[Html](https://hackage.haskell.org/package/type-of-html/docs/Html.html)

Note that you need at least ghc 8.2.

## Typesafety

Part of the html spec is encoded at the typelevel, turning a lot of
mistakes into type errors.

## Performance

`Type of html` is normally a lot faster than `blaze html`.  Criterion
says about the following snippet that `Type of html` needs only ~450
ns, `blaze html` needs ten times more time.

```haskell
example x =
  html_
    ( body_
      ( h1_
        ( img_
        # strong_ "0"
        )
      # div_
        ( div_ "1"
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
```
