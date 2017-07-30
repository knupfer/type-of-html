{-# LANGUAGE ExplicitNamespaces #-}

{-| With type-of-html are three main goals:

* Type safety

* Modularity

* Performance

Let's check out the /type safety/ in ghci:

> Html> td_ (tr_ "a")
>
> <interactive>:1:1: error:
>     • 'Tr is not a valid child of 'Td
>     • In the expression: td_ (tr_ "a")
>       In an equation for ‘it’: it = td_ (tr_ "a")
>
> <interactive>:1:6: error:
>     • 'Tr can't contain a string
>     • In the first argument of ‘td_’, namely ‘(tr_ "a")’
>       In the expression: td_ (tr_ "a")
>       In an equation for ‘it’: it = td_ (tr_ "a")

For every child, it is checked if it could possibly be lawful.

The checking is a bit lenient at the moment:

* some elements can't contain itself as any descendant: at the moment we look only at direct children. This allows some (quite exotic) invalid html documents.
* some elements change their permitted content based on attributes: we don't know at compile time the attributes, therefore we always allow content as if all relevant attributes are set.
* some elements can't be brethren: we look only at parent child relations, therefore if you don't specify the parent, it'll compile

Never the less: these cases are seldom.  In the vast majority of the time you're only allowed to construct valid html.

Let's talk about /modularity/:

Rosetrees of html are just ordinary haskell values which can be composed or abstracted over:

> Html> let table = table_ . map (tr_ . map td_)
> Html> :t table
> table :: ('Td ?> a) => [[a]] -> 'Table > ['Tr > ['Td > a]]
> Html> table [["A","B"],["C"]]
> <table><tr><td>A</td><td>B</td></tr><tr><td>C</td></tr></table>
> Html> import Data.Char
> Html Data.Char> html_ . body_ . table $ map (\c -> [[c], show $ ord c]) ['a'..'d']
> <html><body><tr><td>a</td><td>97</td></tr><tr><td>b</td><td>98</td></tr><tr><td>c</td><td>99</td></tr><tr><td>d</td><td>100</td></tr></body></html>

And here's an example module

@
{\-\# LANGUAGE TypeOperators \#-\}
{\-\# LANGUAGE DataKinds     \#-\}

module Main where

import Html

import Data.Text.Lazy.IO      as T
import Data.Text.Lazy.Builder as T

main :: IO ()
main
  = T.putStrLn
  . T.toLazyText
  . render
  . page
  $ map td_ [1..(10::Int)]

page
  :: 'Tr ?> a
  => a
  -> 'Div
     > ( 'Div > [Char]
       # 'Div > [Char]
       # 'Table > 'Tr > a
       )
page tds =
  div_
    ( div_ "foo"
    # div_ "bar"
    # table_ (tr_ tds)
    )
@

Please note that the type of page is inferable, so ask ghc-mod or
whatever you use to write it for you.  If you choose not to write the
types, you don't need the language extensions.

Last and fast: /performance/!

Don't look any further, there is no option for faster html
generation. type-of-html is 3-20 times faster than blaze-html, which
is until now the fastest generation library and the foundation block
of lucid and shakespeare.

Wait! 3-20 times faster? How is this possible? We supercompile lots of
parts of the generation process. This is possible thanks to the new
features of GHC 8.2: AppendSymbol. We represent tags as kinds and
remove according to the html specification omittable closing tags with
type families. Afterwards we map these tags to (a :: [Symbol]) and
then fold all neighbouring Proxies with AppendSymbol. Afterwards we
retrieve the Proxies with symbolVal which will be embedded in the
executable as CString. All this happens at compile time. At runtime we
do only generate the content and mconcat.

For example, if you write:

> render $ div_ "a"

The compiler does actually optimize it to the following:

> mconcat [ fromString $ symbolVal (Proxy :: Proxy "<div>")
>         , fromString "a"
>         , fromString $ symbolVal (Proxy :: Proxy "</div>")
>         ]

If you write

> render $ div_ (div_ ())

The compiler does actually optimize it to the following:

> mconcat [ fromString $ symbolVal (Proxy :: Proxy "<div><div></div></div>") ]

If you write

> render $ tr_ (td_ "test")

The compiler does actually optimize it to the following:

> mconcat [ fromString $ symbolVal (Proxy :: Proxy "<tr><td>")
>         , fromString "test"
>         , fromString $ symbolVal (Proxy :: Proxy "</tr>")
>         ]

Let's look at core:

We take an extremely simple library

> module Minimal where
>
> import Html
>
> minimal :: String
> minimal = render
>   ( div_ "a"
>   # div_ "b"
>   # table_ (tr_ (td_ "c"))
>   )

compile it with

> ghc -O2 Minimal.hs -ddump-to-file -ddump-simpl -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-uniques

and clean up a bit:

> minimal1 :: Addr#
> minimal1 = "<div>a</div><div>b</div><table><tr><td>c</table>"#
>
> minimal :: String
> minimal = unpackCString# minimal1

Well, that's a perfect optimization! Not only was *all* overhead
removed, optional ending tags were chopped off (tr, td).  This sort of
compiletime optimization isn't for free.  Running ghc with -v says
that desugaring resulted in 675 types and 5507 coercions: Compile
times will increase, some medium size html documents will take 10 secs
to compile.

-}

module Html
  ( render
  , type (>)(..)
  , type (:>)(..)
  , addAttributes
  , type (#)(..)
  , (#)
  , type (?>)
  , Convert
  , Element(..)
  , module Html.Element
  ) where

import Html.Element

import Html.Type
  ( type (>)(..)
  , type (:>)(..)
  , type (?>)
  , type (#)(..)
  , (#)
  , Element(..)
  )

import Html.Function
  ( render
  , addAttributes
  , Convert
  )
