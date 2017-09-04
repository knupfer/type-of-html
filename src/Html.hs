{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE TypeOperators             #-}

{-| type-of-html has three main goals:

* Type safety

* Modularity

* Performance

Let's check out the /type safety/ in ghci:

>>> td_ (tr_ "a")
<BLANKLINE>
<interactive>:1:1: error:
    • 'Tr is not a valid child of 'Td
    • In the expression: td_ (tr_ "a")
      In an equation for ‘it’: it = td_ (tr_ "a")
<BLANKLINE>
<interactive>:1:6: error:
    • 'Tr can't contain a string
    • In the first argument of ‘td_’, namely ‘(tr_ "a")’
      In the expression: td_ (tr_ "a")
      In an equation for ‘it’: it = td_ (tr_ "a")

>>> tr_ (td_ "a")
<tr><td>a</tr>

For every child, it is checked if it could possibly be lawful.

The checking is a bit lenient at the moment:

* some elements can't contain itself as any descendant: at the moment we look only at direct children. This allows some (quite exotic) invalid html documents.
* some elements change their permitted content based on attributes: we don't know at compile time the attributes, therefore we always allow content as if all relevant attributes are set.
* some elements can't be brethren: we look only at parent child relations, therefore if you don't specify the parent, it'll compile

Never the less: these cases are seldom.  In the vast majority of cases you're only allowed to construct valid html.

Let's talk about /modularity/:

Rosetrees of html are just ordinary haskell values which can be composed or abstracted over:

>>> let table = table_ . map (tr_ . map td_)
>>> :t table
table :: ('Td ?> a) => [[a]] -> 'Table > ['Tr > ['Td > a]]
>>> table [["A","B"],["C"]]
<table><tr><td>A<td>B<tr><td>C</table>
>>> import Data.Char
>>> html_ . body_ . table $ map (\c -> [[c], show $ ord c]) ['a'..'d']
<html><body><table><tr><td>a<td>97<tr><td>b<td>98<tr><td>c<td>99<tr><td>d<td>100</table></body></html>

And here's an example module

@
{\-\# LANGUAGE TypeOperators \#-\}
{\-\# LANGUAGE DataKinds     \#-\}

module Main where

import Html

main :: IO ()
main
  = print
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
generation. type-of-html is up to 10 times faster than blaze-html,
which was until now the fastest generation library and the foundation
block of lucid and shakespeare.

Wait! 10 times faster? How is this possible? We supercompile lots of
parts of the generation process. This is possible thanks to the new
features of GHC 8.2: AppendSymbol. We represent tags as kinds and
remove according to the html specification omittable closing tags with
type families. Afterwards we map these tags to (a :: [Symbol]) and
then fold all neighbouring Proxies with AppendSymbol. Afterwards we
retrieve the Proxies with symbolVal which will be embedded in the
executable as CString. All this happens at compile time. At runtime we
do only generate the content and mconcat.

For example, if you write:

> renderText $ tr_ (td_ "test")

The compiler does optimize it to the following (we don't know at
compile time if we need to escape the string):

> mconcat [ Data.Text.Lazy.unpackCString# "<tr><td>"#
>         , escape (Data.Text.Lazy.unpackCString# "test"#)
>         , Data.Text.Lazy.unpackCString# "</tr>"#)
>         ]

If you write

> renderText $ div_ (div_ ())

The compiler does optimize it to the following:

> mconcat [ Data.Text.Lazy.unpackCString# "<div><div></div></div>"# ]

Note that optional ending tags were chopped off (tr, td).  This sort of
compiletime optimization isn't for free, it'll increase compilation times.

-}

module Html
  ( renderString
  , renderText
  , renderByteString
  , type (>)(..)
  , type (:>)(..)
  , addAttributes
  , type (#)(..)
  , (#)
  , type (?>)
  , Raw(..)
  , Convert(..)
  , Element(..)
  , module Html.Element
  ) where

import Html.Reify

import Html.Convert

import Html.Element

import Html.Type

-- | Orphan show instances to faciliate ghci development.
instance                     Document (a > b) String => Show (a > b) where show = renderString
instance {-# OVERLAPPING #-} Document (a > b) String => Show [a > b] where show = renderString
instance                     Document (a:> b) String => Show (a:> b) where show = renderString
instance {-# OVERLAPPING #-} Document (a:> b) String => Show [a:> b] where show = renderString
instance                     Document (a # b) String => Show (a # b) where show = renderString
instance {-# OVERLAPPING #-} Document (a # b) String => Show [a # b] where show = renderString
