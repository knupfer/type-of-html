# Type of html

`Type of html` is a library for generating html in a highly
performant, modular and type safe manner.

Please look at the documentation of the module for an overview of the api:
[Html](https://hackage.haskell.org/package/type-of-html/docs/Html.html)

Note that you need at least ghc 8.2.

## Typesafety

Part of the html spec is encoded at the typelevel, turning a lot of
mistakes into type errors.

Let's check out the /type safety/ in ghci:

```haskell
>>> td_ (tr_ "a")

<interactive>:1:1: error:
    • 'Tr is not a valid child of 'Td
    • In the expression: td_ (tr_ "a")
      In an equation for ‘it’: it = td_ (tr_ "a")

<interactive>:1:6: error:
    • 'Tr can't contain a string
    • In the first argument of ‘td_’, namely ‘(tr_ "a")’
      In the expression: td_ (tr_ "a")
      In an equation for ‘it’: it = td_ (tr_ "a")

>>> tr_ (td_ "a")
<tr><td>a</td></tr>
```

For every child, it is checked if it could possibly be lawful.

The checking is a bit lenient at the moment:

- some elements can't contain itself as any descendant: at the moment we look only at direct children. This allows some (quite exotic) invalid html documents.
- some elements change their permitted content based on attributes: we don't know at compile time the attributes, therefore we always allow content as if all relevant attributes are set.
- some elements can't be brethren: we look only at parent child relations, therefore if you don't specify the parent, it'll compile

Never the less: these cases are seldom.  In the vast majority of cases you're only allowed to construct valid html.

## Modularity

Html documents are just ordinary haskell values which can be composed or abstracted over:

```haskell
>>> let table = table_ . map (tr_ . map td_)
>>> :t table
table :: ('Td ?> a) => [[a]] -> 'Table > ['Tr > ['Td > a]]
>>> table [["A","B"],["C"]]
<table><tr><td>A</td><td>B</td></tr><tr><td>C</td></tr></table>
>>> import Data.Char
>>> html_ . body_ . table $ map (\c -> [[c], show $ ord c]) ['a'..'d']
<html><body><table><tr><td>a</td><td>97</td></tr><tr><td>b</td><td>98</td></tr><tr><td>c</td><td>99</td></tr><tr><td>d</td><td>100</td></tr></table></body></html>
```

And here's an example module:

```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Main where

import Html

import qualified Html.Attribute as A

main :: IO ()
main
  = print
  . page
  $ map td_ [1..(10::Int)]

page
  :: 'Tr ?> a
  => a
  -> 'Div
     :> ( 'Div > [Char]
        # 'Div > [Char]
        # 'Table > 'Tr > a
        )
page tds =
  div_A (A.class_ "qux" <> A.id_ "baz")
    ( div_ "foo"
    # div_ "bar"
    # table_ (tr_ tds)
    )
```

Please note that the type of 'page' is inferable, so ask ghc-mod or
whatever you use to write it for you.  If you choose not to write the
types, you don't need the language extensions.  I strongly suggest
that you don't write signatures for bigger documents.

All text will be automatically html escaped:

```haskell
>>> i_ "&"
<i>&amp;</i>

>>> div_A (A.id_ ">") ()
<div id="&gt;"></div>
```

If you want to opt out, wrap your types into the 'Raw'
constructor. This will increase performance, but can be only used with
trusted input. You can use this e.g. to embed some blaze-html code
into type-of-html.

```haskell
>>> i_ (Raw "</i><script></script><i>")
<i></i><script></script><i></i>
```

## Performance

`Type of html` is a lot faster than `blaze html` or than `lucid`.

Look at the following benchmarks:

Remember this benchmark from blaze?

[blaze](https://jaspervdj.be/blaze/benchmarks.html)

This is comparing blaze with type of html:

![bench-8a453cc](https://user-images.githubusercontent.com/5609565/30251664-8c1f63bc-9664-11e7-84f4-017f6cbc48c6.png)

To look at the exact code generating this code look
[here](bench/Main.hs) in the repo.  The big table benchmark here is
only a 4x4 table. Using a 1000x10 table like on the blaze homepage
yields even better relative performance (~9 times faster), but would
make the other benchmarks unreadable.

How is this possible? We supercompile lots of parts of the generation
process. This is possible thanks to the new features of GHC 8.2:
AppendSymbol. We represent tags as kinds and map these tags to (a ::
[Symbol]) and then fold all neighbouring Proxies with
AppendSymbol. Afterwards we retrieve the Proxies with symbolVal which
will be embedded in the executable as Addr#. All this happens at
compile time. At runtime we do only generate the content and append
Builders.

For example, if you write:

```haskell
renderText $ tr_ (td_ "test")
```

The compiler does optimize it to the following (well, unpackCString#
doesn't exist for Builder, so it's slightly more complicated):

```haskell
decodeUtf8 $ toLazyByteString
  (  Data.ByteString.Builder.unpackCString# "<tr><td>"#
  <> escape (Data.Text.unpackCString# "test"#)
  <> Data.ByteString.Builder.unpackCString# "</tr>"#
  )
```

If you write

```haskell
renderBuilder $ div_ (div_ ())
```

The compiler does optimize it to the following:

```haskell
Data.ByteString.Builder.unpackCString# "<div><div></div></div>"#
```

Note that optional ending tags were chopped off (tr, td).  This sort of
compiletime optimization isn't for free, it'll increase compilation times.

## Comparision to lucid and blaze-html

Advantages of 'type-of-html':
- more or less 5 times faster
- a lot higher type safety: a lot of invalid documents are not inhabited
- fewer dependencies

Disadvantages of 'type-of-html':
- a bit noisy syntax (don't write types!)
- sometimes unusual type error messages
- compile times (1 min for a medium sized page)
- needs at least ghc 8.2

I'd generally recommend that you put your documents into an extra
module to avoid frequent recompilations.  Additionally you can use
type-of-html within an blaze-html document and vice versa.  This
allows you to gradually migrate, or only write the hotpath in a more
efficient representation.

## Example usage

```haskell
module Main where

import Html

import Data.Text.Lazy.IO as TL

main :: IO ()
main = TL.putStrLn $ renderText example

example =
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
              )
            # button_ (i_ "e")
            )
          )
        )
      )
    )
```
