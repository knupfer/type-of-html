{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE CPP                  #-}

module Html.Type.Internal.GHC where

import Html.Type.Internal

import Data.Proxy
import Data.Type.Bool
import GHC.TypeLits

-- | We need efficient cons, snoc and append.  This API has cons(O1)
-- and snoc(O1) but append(On).  Optimal would be a real 2-3
-- FingerTree.
data FingerTree = FingerTree [Sym] Sym
type Empty = 'FingerTree '[] EmptySym
type Split = 'FingerTree '[EmptySym] EmptySym
type NoTail xs = 'FingerTree xs EmptySym
type Singleton = 'FingerTree '[]

type family (<|) s t :: FingerTree where
  (<|) l ('FingerTree (s ': ss) r) = 'FingerTree (AppendSymbol l s ': ss) r
  (<|) l ('FingerTree '[] r) = 'FingerTree '[] (AppendSymbol l r)

type family (|>) t s :: FingerTree where
  (|>) ('FingerTree ss r) rr = 'FingerTree ss (AppendSymbol r rr)

type family (><) t1 t2 :: FingerTree where
  (><) ('FingerTree ss r) ('FingerTree (s ': ss2) r2) = 'FingerTree (Append ss (AppendSymbol r s ': ss2)) r2
  (><) ('FingerTree ss r) ('FingerTree '[] r2) = 'FingerTree ss (AppendSymbol r r2)

#if __GLASGOW_HASKELL__ >= 802
type family OpenTag e where
  OpenTag e = AppendSymbol "<" (AppendSymbol (ShowElement e) ">")

type family CloseTag e where
  CloseTag e = AppendSymbol "</" (AppendSymbol (ShowElement e) ">")

-- | Flatten a document into a type list of tags.
type family ToTypeList a :: FingerTree where
  ToTypeList (a # b)         = ToTypeList a >< ToTypeList b
  ToTypeList ((a :@: ()) ()) = Singleton (If (HasContent (GetInfo a)) (AppendSymbol (OpenTag a) (CloseTag a)) (OpenTag a))
  ToTypeList ((a :@: b) ())  = AppendSymbol "<" (ShowElement a) <| ToTypeList b |> If (HasContent (GetInfo a)) (AppendSymbol ">" (CloseTag a)) ">"
  ToTypeList ((a :@: ()) b)  = OpenTag a <| ToTypeList b |> CloseTag a
  ToTypeList ((a :@: b) c)   = (AppendSymbol "<" (ShowElement a) <| ToTypeList b) >< (">" <| ToTypeList c |> CloseTag a)
  ToTypeList (a := b)        = ShowAttribute a <| ToTypeList b |> "\""
  ToTypeList ()              = Empty
  ToTypeList (Proxy x)       = Singleton x
  ToTypeList x               = Split

type Sym = Symbol
type EmptySym = ""
#else
type family OpenTag e where
  OpenTag e = ["<", ShowElement e, ">"]

type family CloseTag e where
  CloseTag e = ["</", ShowElement e, ">"]

-- | Flatten a document into a type list of tags.
type family ToTypeList a :: FingerTree where
  ToTypeList (a # b)         = ToTypeList a >< ToTypeList b
  ToTypeList ((a :@: ()) ()) = Singleton (If (HasContent (GetInfo a)) (Append (OpenTag a) (CloseTag a)) (OpenTag a))
  ToTypeList ((a :@: b) ())  = '["<", ShowElement a] <| ToTypeList b |> If (HasContent (GetInfo a)) (">" ': CloseTag a) '[">"]
  ToTypeList ((a :@: ()) b)  = OpenTag a <| ToTypeList b |> CloseTag a
  ToTypeList ((a :@: b) c)   = ('["<", ShowElement a] <| ToTypeList b) >< ('[">"] <| ToTypeList c |> CloseTag a)
  ToTypeList (a := b)        = '[ShowAttribute a] <| ToTypeList b |> '["\""]
  ToTypeList ()              = Empty
  ToTypeList (Proxy x)       = Singleton '[x]
  ToTypeList x               = Split

type AppendSymbol xs ys = Append xs ys
type Sym = [Symbol]
type EmptySym = '[]
#endif
