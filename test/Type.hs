{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import GHC.TypeLits
import Html.Type.Internal
import Data.Proxy

main :: IO ()
main = pure ()

  where _x_ = undefined :: Test

type Test =
  ( ToList ()
    == 'List '[] ""
  , ToList Int
    == 'List '[""] ""
  , ToList (Proxy "a")
    == 'List '[] "a"
  , ToList ('A > Char)
    == 'List '["<a>"] "</a>"
  , ToList ('A > Char # 'Div > Int)
    == 'List '["<a>", "</a><div>"] "</div>"
  , ToList (('Div :@: ('ClassA := Int)) Int)
    == 'List '["<div class=\"","\">"] "</div>"
  , ToList (('Div :@: ('ClassA := ())) Int)
    == 'List '["<div class>"] "</div>"
  , ToList (('Div :@: ('ClassA := ())) ())
    == 'List '[] "<div class></div>"
  , ToList (('Div :@: ('ClassA := () # 'IdA := ())) ())
    == 'List '[] "<div class id></div>"
  , ToList (('Div :@: ('ClassA := () # 'IdA := Proxy "ab")) ())
    == 'List '[] "<div class id=\"ab\"></div>"
  )

type family a == b where
  a == a = ()
  a == b = TypeError ('Text "Unequal types:" ':$$: 'ShowType a ':$$: 'ShowType b)
