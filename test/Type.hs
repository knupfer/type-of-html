{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import GHC.TypeLits
import Html.Type.Internal
import Data.Proxy

main :: IO ()
main = pure ()

  where _x_ = undefined :: Test

type Demote (t :: k) = k


type Test =
  ( ToList ()
    == 'List '[] ""
  , ToList Int
    == 'List '[""] ""
  , ToList (Proxy "a")
    == 'List '[] "a"
  , ToList (Demote 'A :> Char)
    == 'List '["<a>"] "</a>"
  , ToList (Demote 'A :> Char # Demote 'Div :> Int)
    == 'List '["<a>", "</a><div>"] "</div>"
  , ToList ((Demote 'Div :@ (Demote 'ClassA := Int)) :> Int)
    == 'List '["<div class=\"","\">"] "</div>"
  , ToList ((Demote 'Div :@ (Demote 'ClassA := ())) :> Int)
    == 'List '["<div class>"] "</div>"
  , ToList ((Demote 'Div :@ (Demote 'ClassA := ())) :> ())
    == 'List '[] "<div class></div>"
  , ToList ((Demote 'Div :@ (Demote 'ClassA := () # Demote 'IdA := ())) :> ())
    == 'List '[] "<div class id></div>"
  , ToList ((Demote 'Div :@ (Demote 'ClassA := () # Demote 'IdA := Proxy "ab")) :> ())
    == 'List '[] "<div class id=\"ab\"></div>"
  )

type family a == b where
  a == a = ()
  a == b = TypeError ('Text "Unequal types:" ':$$: 'ShowType a ':$$: 'ShowType b)
