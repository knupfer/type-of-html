{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE GADTs                  #-}

{-# LANGUAGE CPP #-}

module Html.Type.Internal where

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.Type.Bool
import Data.ByteString (ByteString)

-- |
-- = Type level modelling of html
-- The following types and docs are from the following source:
-- [2020-10-16] https://html.spec.whatwg.org/ Copyright © WHATWG
-- HTML - Living Standard
-- (Apple, Google, Mozilla, Microsoft). This work is licensed under a
-- Creative Commons Attribution 4.0 International License

-- | \ 3 Semantics. structure, and APIs of HTML documents
--     3.2 Elements
--     3.2.5 Content models
data ContentCategory
  = OnlyText
  | (:|:) ContentCategory ContentCategory
  | (:&:) ContentCategory ContentCategory
  | NOT ContentCategory
  | Elements [Symbol]

  -- | \ 3.2.5.1 The "nothing" content model
  | None
  -- | \ 3.2.5.2 Kinds of content
  --     3.2.5.2.1 Metadata content
  | Metadata
  -- | \ 3.2.5.2.2 Flow content
  | Flow
  -- | \ 3.2.5.2.3 Sectioning content
  | Sectioning
  -- | \ 3.2.5.2.4 Heading content
  | Heading
  -- | \ 3.2.5.2.5 Phrasing content
  | Phrasing
  -- | \ 3.2.5.2.6 Embedded content
  | Embedded
  -- | \ 3.2.5.2.7 Interactive content
  | Interactive
  -- | \ 3.2.5.2.8 Palpable content
  | Palpable
  -- | \ 3.2.5.2.9 Script-supporting elements
  | Scripting

data family Element (name :: Symbol) (categories :: [ContentCategory]) (contentModel :: ContentCategory) (contentAttributes :: [Symbol])

data family Attribute (name :: Symbol) (global :: Bool) value

-- |
-- = Utilities

newtype Lawless a = Lawless a

-- | We need efficient cons, snoc and append.  This API has cons(O1)
-- and snoc(O1) but append(On).  Optimal would be a FingerTree.
data List = List [Symbol] Symbol

type family (<|) s t :: List where
  (<|) l ('List (s ': ss) r) = 'List (AppendSymbol l s ': ss) r
  (<|) l ('List '[] r) = 'List '[] (AppendSymbol l r)

type family (|>) t s :: List where
  (|>) ('List ss r) rr = 'List ss (AppendSymbol r rr)

type family (><) t1 t2 :: List where
  (><) ('List ss r) ('List (s ': ss2) r2) = 'List (Append ss (AppendSymbol r s ': ss2)) r2
  (><) ('List ss r) ('List '[] r2) = 'List ss (AppendSymbol r r2)

-- | Flatten a document into a type list of tags.
type family ToList a :: List where
  ToList (Element name categories contentModel contentAttributes :> b)   = AppendSymbol "<" (AppendSymbol name ">") <| ToList b |> AppendSymbol "</" (AppendSymbol name ">")
  ToList ((Element name categories contentModel contentAttributes :@ at) :> b)   = AppendSymbol "<" name <| ToList at >< (">" <| ToList b) |> AppendSymbol "</" (AppendSymbol name ">")
  ToList (Element name categories None contentAttributes)   = 'List '[] (AppendSymbol "<" (AppendSymbol name ">"))
  ToList (Element name categories contentModel contentAttributes)   = ToList (Element name categories contentModel contentAttributes :> ())
  ToList (Element name categories None contentAttributes :@ at)   = AppendSymbol "<" name <| ToList at |> ">"
  ToList (Element name categories contentModel contentAttributes :@ at)   = AppendSymbol "<" name <| ToList at |> AppendSymbol "></" (AppendSymbol name ">")
  ToList (a # b)         = ToList a >< ToList b
  ToList (Lawless a)     = ToList a
  ToList (Attribute a global ())       = 'List '[] (AppendSymbol " " a)
  ToList (Attribute a global b)        = AppendSymbol " " (AppendSymbol a "=\"") <| ToList b |> "\""
  ToList ()              = 'List '[] ""
  ToList (Proxy x)       = 'List '[] x
  ToList x               = 'List '[""] ""

-- | Combine two elements or attributes sequentially.
--
-- >>> I # Div
-- <i></i><div></div>
--
-- >>> I :@ (IdA "a" # ClassA "b") :> "c"
-- <i id="a" class="b">c</i>
data (#) a b = (:#:) a b
{-# INLINE (#) #-}
(#) :: a -> b -> a # b
(#) = (:#:)
infixr 5 #

type family Lawful relationship father child :: Constraint where
  Lawful relation x (Raw y) = ()
  Lawful relation x (Lawless y) = ()
  Lawful relation x (y1 # y2) = (Lawful relation x y1, Lawful relation x y2)
  Lawful relation x (Maybe y) = Lawful relation x y
  Lawful relation x (Either y1 y2) = (Lawful relation x y1, Lawful relation x y2)
  Lawful relation x [y] = Lawful relation x y
  Lawful relation x (c :@ _) = Lawful relation x c
  Lawful relation x (c :> _) = Lawful relation x c

  Lawful AttributeValue (Attribute name1 global1 value1) (Attribute name2 global2 value2) = TypeError (Text "The attribute " :<>: Text name1 :<>: Text " can't contain the attribute " :<>: Text name2 :<>: Text ".")
  Lawful AttributeValue (Attribute name1 global1 value1) (Element name2 categories contentModel contentAttributes) = TypeError (Text "The attribute " :<>: Text name1 :<>: Text " can't contain the element " :<>: Text name2 :<>: Text ".")
  Lawful AttributeValue _ _ = (() :: Constraint)

  Lawful Fatherhood (e :@ _) c = Lawful Fatherhood e c
  Lawful Fatherhood (Element name categories contentModel contentAttributes) (Attribute name2 global value) = TypeError (Text name :<>: Text " can't have an attribute as children.")
  Lawful Fatherhood (Element name categories None contentAttributes) _ = TypeError (Text name :<>: Text " can't have children.")

  Lawful Fatherhood (Element name1 categories1 contentModel1 contentAttributes1)
               (Element name2 categories2 contentModel2 contentAttributes2) = MaybeTypeError name1 (Text name2) (CheckContentCategory name2 contentModel1 categories2)
  Lawful Fatherhood (Element name categories contentModel contentAttributes) string = MaybeTypeError name (ShowType string) (CheckContentCategory "" contentModel '[OnlyText, Flow, Phrasing])
  Lawful Fatherhood _ _ = TypeError (Text "Only Elements and Elements with Attributes can father children.")

  Lawful Attribution (Element name categories contentModel contentAttributes) (Attribute a global value)
    = If (global || Elem a contentAttributes)
    (Lawful AttributeValue (Attribute a global value) value)
    (TypeError (Text a :<>: Text " is not a valid attribute of " :<>: Text name :<>: Text "."))
  Lawful Attribution (Element name categories contentModel contentAttributes) a = TypeError (ShowType a :<>: Text " is not a valid attribute of " :<>: Text name :<>: Text ".")
  Lawful Attribution a _ = TypeError (ShowType a :<>: Text " is not an attributable element.")

type family MaybeTypeError a b c where
  MaybeTypeError a b c = If c (() :: Constraint)
   (TypeError (b :<>: Text " is not a valid child of " :<>: Text a :<>: Text "."))

data Relationship
  = Fatherhood
  | Attribution
  | AttributeValue

data (:>) father child where
  (:>) :: Lawful Fatherhood f c => f -> c -> f :> c

data (:@) element attribution where
  (:@) :: Lawful Attribution e a => e -> a -> e :@ a

infixr 6 :>
infixr 9 :@

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw {fromRaw :: a}

type family Null xs where
  Null '[] = True
  Null _ = False

type family Length c where
  Length (a :> b) = Length a + Length b
  Length (a :@ b) = Length b
  Length (a # b)       = Length a + Length b
  Length (Lawless a)   = Length a
  Length (Attribute a global b) = Length b
  Length (Element name categories contentModel contentAttributes) = 0
  Length ()            = 0
  Length (Proxy _)     = 0
  Length _             = 1

-- | Append two type lists.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Append xs ys :: [k] where

  Append (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': xs) ys
        = x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': Append xs ys

  Append (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': xs) ys
        = x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Append xs ys

  Append (x1 ': x2 ': x3 ': x4 ': xs) ys
        = x1 ': x2 ': x3 ': x4 ': Append xs ys

  Append (x1 ': x2 ': xs) ys
        = x1 ': x2 ': Append xs ys

  Append (x1 ': xs) ys
        = x1 ': Append xs ys

  Append '[] ys
        = ys

-- | Type level drop.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Drop n xs :: [k] where
  Drop 0 xs = xs
  Drop 1 (_ ': xs) = xs
  Drop 2 (_ ': _ ': xs) = xs
  Drop 4 (_ ': _ ': _ ': _ ': xs) = xs
#if __GLASGOW_HASKELL__ >= 804
  Drop 8 (_ ': _ ': _ ': _ ': _ ': _ ': _ ': _ ': xs) = xs
  Drop n xs = Drop (n - 2^Log2 n) (Drop (2^(Log2 n-1)) (Drop (2^(Log2 n-1)) xs))
#else
  Drop 3 (_ ': _ ': _ ': xs) = xs
  Drop n (_ ': _ ': _ ': _ ': _ ': xs) = Drop (n-5) xs
#endif

-- | Type level take.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Take n xs :: [k] where
  Take 0 _ = '[]
  Take 1 (x1 ': _) = '[x1]
  Take 2 (x1 ': x2 ': _) = '[x1, x2]
  Take 3 (x1 ': x2 ': x3 ': _) = '[x1, x2, x3]
  Take 4 (x1 ': x2 ': x3 ': x4 ': _) = '[x1, x2, x3, x4]
  Take 5 (x1 ': x2 ': x3 ': x4 ': x5 ': _) = '[x1, x2, x3, x4, x5]
  Take 6 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': _) = '[x1, x2, x3, x4, x5, x6]
  Take 7 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': _) = '[x1, x2, x3, x4, x5, x6, x7]
  Take 8 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': _) = '[x1, x2, x3, x4, x5, x6, x7, x8]
  Take 9 (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': _) = '[x1, x2, x3, x4, x5, x6, x7, x8, x9]
  Take n (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': xs) = x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': Take (n-10) xs

type family CheckContentCategory (name :: Symbol) (a :: ContentCategory) (b :: [ContentCategory]) :: Bool where
  CheckContentCategory n (a :|: b) c     = CheckContentCategory n a c || CheckContentCategory n b c
  CheckContentCategory n (a :&: b) c     = CheckContentCategory n a c && CheckContentCategory n b c
  CheckContentCategory n (NOT a) c       = Not (CheckContentCategory n a c)
  CheckContentCategory n (Elements xs) c = Elem n xs
  CheckContentCategory n a c             = Elem a c

infixr 2 :|:
infixr 3 :&:

type family Elem (a :: k) (xs :: [k]) where
  Elem a (a : xs) = True
  Elem a (_ : xs) = Elem a xs
  Elem a '[]      = False

newtype T (proxies :: k) target = T target

-- | Data for declaring variables in a html document which will be compacted.
data V (n :: Symbol) = V

newtype One a = One a

-- | Unique set of variables in a html document in the order of occurence.
type Variables a = Dedupe (GetV a)

-- | A compacted html documented with it's variables annoted as a list
-- of Symbols.  It's Show instance is quite useful for developping: It
-- highlights variables and renders the rest of the html.
data CompactHTML (a :: [Symbol]) = MkCompactHTML ByteString [(Int, ByteString)]

instance ShowTypeList a => Show (CompactHTML a) where
  show (MkCompactHTML bs xs) = show bs ++ foldMap (\(i,b) -> "\n\ESC[36m" ++ vars !! i ++ "\ESC[0m\n" ++ show b) xs
    where vars = showTypeList @a

type family GetV a :: [Symbol] where
  GetV (a # b)       = Append (GetV a) (GetV b)
  GetV (a :> b)      = Append (GetV a) (GetV b)
  GetV (a :@ b)      = Append (GetV a) (GetV b)
  GetV (Attribute a global b) = GetV b
  GetV (Maybe a)     = GetV a
  GetV [a]           = GetV a
  GetV (Either a b)  = Append (GetV a) (GetV b)
  GetV (V v)         = '[v]
  GetV x             = '[]

type family Dedupe xs :: [Symbol] where
  Dedupe (x ': xs) = x ': Dedupe (Delete x xs)
  Dedupe '[] = '[]

type family Delete x xs :: [Symbol] where
  Delete x (x ': xs) = Delete x xs
  Delete x (y ': xs) = y ': Delete x xs
  Delete _ _ = '[]

class ShowTypeList a where
  showTypeList :: [String]

instance (ShowTypeList xs, KnownSymbol x) => ShowTypeList (x ': xs) where
  showTypeList = symbolVal (Proxy @x) : showTypeList @xs

instance ShowTypeList '[] where
  showTypeList = []
