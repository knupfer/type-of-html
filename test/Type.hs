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
    == Empty
  , ToList Int
    == Split
  , ToList (Proxy "a")
    == 'FingerTree '[] "a"
  , ToList ('A > Char)
    == 'FingerTree '["<a>"] "</a>"
  , ToList ('A > Char # 'Div > Int)
    == 'FingerTree '["<a>", "</a><div>"] "</div>"
  , ToList (('Div :@: ('ClassA := Int)) Int)
    == 'FingerTree '["<div class=\"","\">"] "</div>"
  , ToList (('Div :@: ('ClassA := ())) Int)
    == 'FingerTree '["<div class=\"\">"] "</div>"
  , ToList (('Div :@: ('ClassA := ())) ())
    == 'FingerTree '[] "<div class=\"\"></div>"
  , ToList (('Div :@: ('ClassA := () # 'IdA := ())) ())
    == 'FingerTree '[] "<div class=\"\" id=\"\"></div>"
  , ToList (('Div :@: ('ClassA := () # 'IdA := Proxy "ab")) ())
    == 'FingerTree '[] "<div class=\"\" id=\"ab\"></div>"
  )

type family a == b where
  a == a = ()
  a == b = TypeError ('Text "Unequal types:" ':$$: 'ShowType a ':$$: 'ShowType b)
