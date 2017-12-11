{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE GADTs                  #-}

module Html.Type.Internal where

import Html.CPP

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Bool

{-# DEPRECATED

  Acronym   ,
  Applet    ,
  Basefont  ,
  Big       ,
  Blink     ,
  Center    ,
  Command   ,
  Content   ,
  Dir       ,
  Font      ,
  Frame     ,
  Frameset  ,
  Isindex   ,
  Keygen    ,
  Listing   ,
  Marquee   ,
  Multicol  ,
  Noembed   ,
  Plaintext ,
  Shadow    ,
  Spacer    ,
  Strike    ,
  Tt        ,
  Xmp       ,
  Nextid

 "This is an obsolete html element and should not be used." #-}

-- | The data type of all html elements and the kind of elements.
data Element
  = DOCTYPE

  | A
  | Abbr
  | Acronym
  | Address
  | Applet
  | Area
  | Article
  | Aside
  | Audio
  | B
  | Base
  | Basefont
  | Bdi
  | Bdo
  | Bgsound
  | Big
  | Blink
  | Blockquote
  | Body
  | Br
  | Button
  | Canvas
  | Caption
  | Center
  | Cite
  | Code
  | Col
  | Colgroup
  | Command
  | Content
  | Data
  | Datalist
  | Dd
  | Del
  | Details
  | Dfn
  | Dialog
  | Dir
  | Div
  | Dl
  | Dt
  | Element
  | Em
  | Embed
  | Fieldset
  | Figcaption
  | Figure
  | Font
  | Footer
  | Form
  | Frame
  | Frameset
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Head
  | Header
  | Hgroup
  | Hr
  | Html
  | I
  | Iframe
  | Image
  | Img
  | Input
  | Ins
  | Isindex
  | Kbd
  | Keygen
  | Label
  | Legend
  | Li
  | Link
  | Listing
  | Main
  | Map
  | Mark
  | Marquee
  | Math
  | Menu
  | Menuitem
  | Meta
  | Meter
  | Multicol
  | Nav
  | Nextid
  | Nobr
  | Noembed
  | Noframes
  | Noscript
  | Object
  | Ol
  | Optgroup
  | Option
  | Output
  | P
  | Param
  | Picture
  | Plaintext
  | Pre
  | Progress
  | Q
  | Rp
  | Rt
  | Rtc
  | Ruby
  | S
  | Samp
  | Script
  | Section
  | Select
  | Shadow
  | Slot
  | Small
  | Source
  | Spacer
  | Span
  | Strike
  | Strong
  | Style
  | Sub
  | Summary
  | Sup
  | Svg
  | Table
  | Tbody
  | Td
  | Template
  | Textarea
  | Tfoot
  | Th
  | Thead
  | Time
  | Title
  | Tr
  | Track
  | Tt
  | U
  | Ul
  | Var
  | Video
  | Wbr
  | Xmp

data Attribute
  = AcceptA
  | AcceptCharsetA
  | AccesskeyA
  | ActionA
  | AllowfullscreenA
  | AllowpaymentrequestA
  | AlignA
  | AltA
  | AsyncA
  | AutocompleteA
  | AutofocusA
  | AutoplayA
  | AutosaveA
  | BgcolorA
  | BorderA
  | BufferedA
  | ChallengeA
  | CharsetA
  | CheckedA
  | CiteA
  | ClassA
  | CodeA
  | CodebaseA
  | ColorA
  | ColsA
  | ColspanA
  | ContentA
  | ContenteditableA
  | ContextmenuA
  | ControlsA
  | CoordsA
  | CrossoriginA
  | DataA
  | DatetimeA
  | DefaultA
  | DeferA
  | DirA
  | DirnameA
  | DisabledA
  | DownloadA
  | DraggableA
  | DropzoneA
  | EnctypeA
  | ForA
  | FormA
  | FormactionA
  | FormenctypeA
  | FormmethodA
  | FormnovalidateA
  | FormtargetA
  | HeadersA
  | HeightA
  | HiddenA
  | HighA
  | HrefA
  | HreflangA
  | HttpEquivA
  | IconA
  | IdA
  | IntegrityA
  | IsmapA
  | ItempropA
  | KeytypeA
  | KindA
  | LabelA
  | LangA
  | LanguageA
  | ListA
  | LongdescA
  | LoopA
  | LowA
  | ManifestA
  | MaxA
  | MaxlengthA
  | MediaA
  | MethodA
  | MinA
  | MinlengthA
  | MultipleA
  | MutedA
  | NameA
  | NonceA
  | NovalidateA
  | OpenA
  | OptimumA
  | PatternA
  | PingA
  | PlaceholderA
  | PosterA
  | PreloadA
  | RadiogroupA
  | ReadonlyA
  | ReferrerpolicyA
  | RelA
  | RequiredA
  | RevA
  | ReversedA
  | RowsA
  | RowspanA
  | SandboxA
  | ScopeA
  | ScopedA
  | SeamlessA
  | SelectedA
  | ShapeA
  | SizeA
  | SizesA
  | SlotA
  | SpanA
  | SpellcheckA
  | SrcA
  | SrcdocA
  | SrclangA
  | SrcsetA
  | StartA
  | StepA
  | StyleA
  | SummaryA
  | TabindexA
  | TargetA
  | TitleA
  | TranslateA
  | TypeA
  | TypemustmatchA
  | UsemapA
  | ValueA
  | WidthA
  | WrapA

-- | We need efficient cons, snoc and append.  This API has cons(O1)
-- and snoc(O1) but append(On).  Optimal would be a real 2-3
-- FingerTree.
data FingerTree = FingerTree [Sym] Sym
type Empty = 'FingerTree '[] EmptySym
type Split = 'FingerTree '[EmptySym] EmptySym
type NoTail xs = 'FingerTree xs EmptySym
type Singleton = 'FingerTree '[]

type family (<|) s t :: FingerTree where
  (<|) l ('FingerTree (s ': ss) r) = 'FingerTree (AppSymbols (Append (List l) (List s)) ': ss) r
  (<|) l ('FingerTree '[] r) = 'FingerTree '[] (AppSymbols (Append (List l) (List r)))

type family (|>) t s :: FingerTree where
  (|>) ('FingerTree ss r) rr = 'FingerTree ss (AppSymbols (Append (List r) (List rr)))

type family (><) t1 t2 :: FingerTree where
  (><) ('FingerTree ss r) ('FingerTree (s ': ss2) r2) = 'FingerTree (Append ss (AppSymbols (Append (List r) (List s)) ': ss2)) r2
  (><) ('FingerTree ss r) ('FingerTree '[] r2) = 'FingerTree ss (AppSymbols (Append (List r) (List r2)))

type family OpenTag e where
  OpenTag e = ["<", ShowElement e, ">"]

type family CloseTag e where
  CloseTag e = ["</", ShowElement e, ">"]

-- | Flatten a document into a type list of tags.
type family ToList a :: FingerTree where
  ToList (a # b)         = ToList a >< ToList b
  ToList ((a :@: ()) ()) = Singleton (If (HasContent (GetEInfo a)) (AppSymbols (Append (OpenTag a) (CloseTag a))) (AppSymbols (OpenTag a)))
  ToList ((a :@: b) ())  = AppSymbols '["<", ShowElement a] <| ToList b |> If (HasContent (GetEInfo a)) (AppSymbols (">" ': CloseTag a)) (AppSymbols '[">"])
  ToList ((a :@: ()) b)  = AppSymbols (OpenTag a) <| ToList b |> AppSymbols (CloseTag a)
  ToList ((a :@: b) c)   = (AppSymbols '["<", ShowElement a] <| ToList b) >< (AppSymbols '[">"] <| ToList c |> AppSymbols (CloseTag a))
  ToList (a := b)        = AppSymbols '[" ", ShowAttribute a,"=\""] <| ToList b |> AppSymbols '["\""]
  ToList ()              = Empty
  ToList (Proxy x)       = Singleton (AppSymbols '[x])
  ToList x               = Split

newtype (:=) (a :: Attribute) b = AT b

-- | Check whether `b` is a valid child of `a`.  You'll propably never
-- need to call this directly.  Through a GADT, it is enforced that
-- every child is lawful.
type family (a :: Element) ?> b :: Constraint where
  a ?> (b :@: _) _     = MaybeTypeError a b (CheckContentCategory (EInfoContent (GetEInfo a)) (SingleElement b ': EInfoCategories (GetEInfo b)))
  a ?> (b # c)         = (a ?> b, a ?> c)
  a ?> Maybe b         = a ?> b
  a ?> Either b c      = (a ?> b, a ?> c)
  a ?> f ((b :@: c) d) = a ?> (b :@: c) d
  a ?> f (b # c)       = a ?> (b # c)
  a ?> ()              = ()
  a ?> (b -> c)        = TypeError (Text "Html elements can't contain functions")
  a ?> b               = CheckString a b

type family (a :: Element) ??> b :: Constraint where
  a ??> () = ()
  a ??> (b # c)  = (a ??> b, a ??> c)
  a ??> (b := _) = If (Elem a (AInfoElements (GetAInfo b)) || Null (AInfoElements (GetAInfo b)))
                   (() :: Constraint)
                   (TypeError (ShowType b :<>: Text " is not a valid attribute of " :<>: ShowType a))
  a ??> b        = TypeError (ShowType b :<>: Text " is not an attribute.")

-- | Combine two elements or attributes sequentially.
--
-- >>> i_ () # div_ ()
-- <i></i><div></div>
--
-- >>> i_A (A.id_ "a" # A.class_ "b") "c"
-- <i id="a" class="b">c</i>
data (#) a b = (:#:) a b
{-# INLINE (#) #-}
(#) :: a -> b -> a # b
(#) = (:#:)
infixr 5 #

-- | Type synonym for elements without attributes.
type (>) a b = (:@:) a () b

-- | Decorate an element with attributes and descend to a valid child.
-- It is recommended to use the predefined elements.
--
-- >>> WithAttributes (A.class_ "bar") "a" :: ('Div :@: ('ClassA := String)) String
-- <div class="bar">a</div>
--
-- >>> div_A (A.class_ "bar") "a"
-- <div class="bar">a</div>
--
-- >>> div_ "a"
-- <div>a</div>
data (:@:) (a :: Element) b c where
  WithAttributes :: (a ??> b, a ?> c) => b -> c -> (a :@: b) c
infixr 8 :@:

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw a

  -------------------
  -- internal code --
  -------------------

type family Null xs where
  Null '[] = True
  Null _ = False

type family Length c where
  Length (a # b)       = Length a + Length b
  Length ((_ :@: b) c) = Length b + Length c
  Length (_ := b)      = Length b
  Length ()            = 0
  Length (Proxy _)     = 0
  Length _             = 1

-- | Check whether an element may have content.
type family HasContent a where
  HasContent (EInfo _ _ NoContent) = False
  HasContent _                     = True

-- | Append two type lists.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Append xs ys :: [k] where

  Append (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': x33 ': x34 ': x35 ': x36 ': x37 ': x38 ': x39 ': x40 ': x41 ': x42 ': x43 ': x44 ': x45 ': x46 ': x47 ': x48 ': x49 ': x50 ': x51 ': x52 ': x53 ': x54 ': x55 ': x56 ': x57 ': x58 ': x59 ': x60 ': x61 ': x62 ': x63 ': x64 ': xs) ys
        = x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': x33 ': x34 ': x35 ': x36 ': x37 ': x38 ': x39 ': x40 ': x41 ': x42 ': x43 ': x44 ': x45 ': x46 ': x47 ': x48 ': x49 ': x50 ': x51 ': x52 ': x53 ': x54 ': x55 ': x56 ': x57 ': x58 ': x59 ': x60 ': x61 ': x62 ': x63 ': x64 ': Append xs ys

  Append (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': xs) ys
        = x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': x9 ': x10 ': x11 ': x12 ': x13 ': x14 ': x15 ': x16 ': x17 ': x18 ': x19 ': x20 ': x21 ': x22 ': x23 ': x24 ': x25 ': x26 ': x27 ': x28 ': x29 ': x30 ': x31 ': x32 ': Append xs ys

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
  Drop 3 (_ ': _ ': _ ': xs) = xs
  Drop 4 (_ ': _ ': _ ': _ ': xs) = xs
  Drop n (_ ': _ ': _ ': _ ': _ ': xs) = Drop (n-5) xs

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
  Take n (x1 ': x2 ': x3 ': x4 ': x5 ': xs) = x1 ': x2 ': x3 ': x4 ': x5 ': Take (n-5) xs

-- | Type of type level information about tags.
data EInfo
  (name :: Symbol)
  (contentCategories :: [ContentCategory])
  (permittedContent  :: ContentCategory)

type family EInfoName x       where EInfoName       (EInfo n _ _)  = n
type family EInfoCategories x where EInfoCategories (EInfo _ cs _) = cs
type family EInfoContent x    where EInfoContent    (EInfo _ _ c)  = c

type ShowElement (x :: Element) = EInfoName (GetEInfo x)

type family CheckContentCategory (a :: ContentCategory) (b :: [ContentCategory]) :: Bool where
  CheckContentCategory (a :|: b) c = CheckContentCategory a c || CheckContentCategory b c
  CheckContentCategory (a :&: b) c = CheckContentCategory a c && CheckContentCategory b c
  CheckContentCategory (NOT a) c   = Not (CheckContentCategory a c)
  CheckContentCategory a c         = Elem a c

-- | Check whether a given element may contain a string.
type family CheckString (a :: Element) b where
  CheckString a b = If (CheckContentCategory (EInfoContent (GetEInfo a)) '[OnlyText, FlowContent, PhrasingContent])
                       (() :: Constraint)
                       (TypeError (ShowType a :<>: Text " can't contain a " :<>: ShowType b))

-- | Content categories according to the html spec.
data ContentCategory
  = MetadataContent
  | FlowContent
  | SectioningContent
  | HeadingContent
  | PhrasingContent
  | (:|:) ContentCategory ContentCategory
  | (:&:) ContentCategory ContentCategory
  | NOT ContentCategory
  | NoContent
  | OnlyText
  | SingleElement Element

infixr 2 :|:
infixr 3 :&:

type family MaybeTypeError (a :: Element) (b :: Element) c where
  MaybeTypeError a b c = If c (() :: Constraint)
   (TypeError (ShowType b :<>: Text " is not a valid child of " :<>: ShowType a))

type family Elem (a :: k) (xs :: [k]) where
  Elem a (a : xs) = True
  Elem a (_ : xs) = Elem a xs
  Elem a '[]      = False

newtype T (proxies :: k) target = T target

data AInfo (name :: Symbol) (elements :: [Element])
type ShowAttribute (x :: Attribute) = AInfoName (GetAInfo x)
type family AInfoElements x where AInfoElements (AInfo _ es) = es
type family AInfoName x where AInfoName (AInfo s _) = s

-- | Get type list of valid elements for a given attribute.  An empty list signifies global attribute.
type family GetAInfo a = r | r -> a where
  GetAInfo AcceptA              = AInfo "accept" '[Form, Input]
  GetAInfo AcceptCharsetA       = AInfo "accept-charset" '[Form]
  GetAInfo AccesskeyA           = AInfo "accesskey" '[]
  GetAInfo ActionA              = AInfo "action" '[Form]
  GetAInfo AllowfullscreenA     = AInfo "allowfullscreen" '[Iframe]
  GetAInfo AllowpaymentrequestA = AInfo "allowpaymentrequest" '[Iframe]
  GetAInfo AlignA               = AInfo "align" '[Applet, Caption, Col, Colgroup, Hr, Iframe, Img, Table, Tbody, Td, Tfoot, Th, Thead, Tr]
  GetAInfo AltA                 = AInfo "alt" '[Applet, Area, Img, Input]
  GetAInfo AsyncA               = AInfo "async" '[Script]
  GetAInfo AutocompleteA        = AInfo "autocomplete" '[Form, Input]
  GetAInfo AutofocusA           = AInfo "autofocus" '[Button, Input, Keygen, Select, Textarea]
  GetAInfo AutoplayA            = AInfo "autoplay" '[Audio, Video]
  GetAInfo AutosaveA            = AInfo "autosave" '[Input]
  GetAInfo BgcolorA             = AInfo "bgcolor" '[Body, Col, Colgroup, Marquee, Table, Tbody, Tfoot, Td, Th, Tr]
  GetAInfo BorderA              = AInfo "border" '[Img, Object, Table]
  GetAInfo BufferedA            = AInfo "buffered" '[Audio, Video]
  GetAInfo ChallengeA           = AInfo "challenge" '[Keygen]
  GetAInfo CharsetA             = AInfo "charset" '[Meta, Script]
  GetAInfo CheckedA             = AInfo "checked" '[Command, Input]
  GetAInfo CiteA                = AInfo "cite" '[Blockquote, Del, Ins, Q]
  GetAInfo ClassA               = AInfo "class" '[]
  GetAInfo CodeA                = AInfo "code" '[Applet]
  GetAInfo CodebaseA            = AInfo "codebase" '[Applet]
  GetAInfo ColorA               = AInfo "color" '[Basefont, Font, Hr]
  GetAInfo ColsA                = AInfo "cols" '[Textarea]
  GetAInfo ColspanA             = AInfo "colspan" '[Td, Th]
  GetAInfo ContentA             = AInfo "content" '[Meta]
  GetAInfo ContenteditableA     = AInfo "contenteditable" '[]
  GetAInfo ContextmenuA         = AInfo "contextmenu" '[]
  GetAInfo ControlsA            = AInfo "controls" '[Audio, Video]
  GetAInfo CoordsA              = AInfo "coords" '[Area]
  GetAInfo CrossoriginA         = AInfo "crossorigin" '[Audio, Img, Link, Script, Video]
  GetAInfo DataA                = AInfo "data" '[Object]
  GetAInfo DatetimeA            = AInfo "datetime" '[Del, Ins, Time]
  GetAInfo DefaultA             = AInfo "default" '[Track]
  GetAInfo DeferA               = AInfo "defer" '[Script]
  GetAInfo DirA                 = AInfo "dir" '[]
  GetAInfo DirnameA             = AInfo "dirname" '[Input, Textarea]
  GetAInfo DisabledA            = AInfo "disabled" '[Button, Command, Fieldset, Input, Keygen, Optgroup, Option, Select, Textarea]
  GetAInfo DownloadA            = AInfo "download" '[A, Area]
  GetAInfo DraggableA           = AInfo "draggable" '[]
  GetAInfo DropzoneA            = AInfo "dropzone" '[]
  GetAInfo EnctypeA             = AInfo "enctype" '[Form]
  GetAInfo ForA                 = AInfo "for" '[Label, Output]
  GetAInfo FormA                = AInfo "form" '[Button, Fieldset, Input, Keygen, Label, Meter, Object, Output, Progress, Select, Textarea]
  GetAInfo FormactionA          = AInfo "formaction" '[Input, Button]
  GetAInfo FormenctypeA         = AInfo "formenctype" '[Button, Input]
  GetAInfo FormmethodA          = AInfo "formmethod" '[Button, Input]
  GetAInfo FormnovalidateA      = AInfo "formnovalidate" '[Button, Input]
  GetAInfo FormtargetA          = AInfo "formtarget" '[Button, Input]
  GetAInfo HeadersA             = AInfo "headers" '[Td, Th]
  GetAInfo HeightA              = AInfo "height" '[Canvas, Embed, Iframe, Img, Input, Object, Video]
  GetAInfo HiddenA              = AInfo "hidden" '[]
  GetAInfo HighA                = AInfo "high" '[Meter]
  GetAInfo HrefA                = AInfo "href" '[A, Area, Base, Link]
  GetAInfo HreflangA            = AInfo "hreflang" '[A, Area, Link]
  GetAInfo HttpEquivA           = AInfo "httpEquiv" '[Meta]
  GetAInfo IconA                = AInfo "icon" '[Command]
  GetAInfo IdA                  = AInfo "id" '[]
  GetAInfo IntegrityA           = AInfo "integrity" '[Link, Script]
  GetAInfo IsmapA               = AInfo "ismap" '[Img]
  GetAInfo ItempropA            = AInfo "itemprop" '[]
  GetAInfo KeytypeA             = AInfo "keytype" '[Keygen]
  GetAInfo KindA                = AInfo "kind" '[Track]
  GetAInfo LabelA               = AInfo "label" '[Track]
  GetAInfo LangA                = AInfo "lang" '[]
  GetAInfo LanguageA            = AInfo "language" '[Script]
  GetAInfo ListA                = AInfo "list" '[Input]
  GetAInfo LongdescA            = AInfo "longdesc" '[Img]
  GetAInfo LoopA                = AInfo "loop" '[Audio, Bgsound, Marquee, Video]
  GetAInfo LowA                 = AInfo "low" '[Meter]
  GetAInfo ManifestA            = AInfo "manifest" '[Html]
  GetAInfo MaxA                 = AInfo "max" '[Input, Meter, Progress]
  GetAInfo MaxlengthA           = AInfo "maxlength" '[Input, Textarea]
  GetAInfo MediaA               = AInfo "media" '[A, Area, Link, Source, Style]
  GetAInfo MethodA              = AInfo "method" '[Form]
  GetAInfo MinA                 = AInfo "min" '[Input, Meter]
  GetAInfo MinlengthA           = AInfo "minlength" '[Input, Textarea]
  GetAInfo MultipleA            = AInfo "multiple" '[Input, Select]
  GetAInfo MutedA               = AInfo "muted" '[Video]
  GetAInfo NameA                = AInfo "name" '[Button, Form, Fieldset, Iframe, Input, Keygen, Object, Output, Select, Textarea, Map, Meta, Param]
  GetAInfo NonceA               = AInfo "nonce" '[Link, Script, Style]
  GetAInfo NovalidateA          = AInfo "novalidate" '[Form]
  GetAInfo OpenA                = AInfo "open" '[Details]
  GetAInfo OptimumA             = AInfo "optimum" '[Meter]
  GetAInfo PatternA             = AInfo "pattern" '[Input]
  GetAInfo PingA                = AInfo "ping" '[A, Area]
  GetAInfo PlaceholderA         = AInfo "placeholder" '[Input, Textarea]
  GetAInfo PosterA              = AInfo "poster" '[Video]
  GetAInfo PreloadA             = AInfo "preload" '[Audio, Video]
  GetAInfo RadiogroupA          = AInfo "radiogroup" '[Command]
  GetAInfo ReadonlyA            = AInfo "readonly" '[Input, Textarea]
  GetAInfo ReferrerpolicyA      = AInfo "referrerpolicy" '[A, Area, Iframe, Img, Link]
  GetAInfo RelA                 = AInfo "rel" '[A, Area, Link]
  GetAInfo RequiredA            = AInfo "required" '[Input, Select, Textarea]
  GetAInfo RevA                 = AInfo "rev" '[A, Link]
  GetAInfo ReversedA            = AInfo "reversed" '[Ol]
  GetAInfo RowsA                = AInfo "rows" '[Textarea]
  GetAInfo RowspanA             = AInfo "rowspan" '[Td, Th]
  GetAInfo SandboxA             = AInfo "sandbox" '[Iframe]
  GetAInfo ScopeA               = AInfo "scope" '[Th]
  GetAInfo ScopedA              = AInfo "scoped" '[Style]
  GetAInfo SeamlessA            = AInfo "seamless" '[Iframe]
  GetAInfo SelectedA            = AInfo "selected" '[Option]
  GetAInfo ShapeA               = AInfo "shape" '[A, Area]
  GetAInfo SizeA                = AInfo "size" '[Input, Select]
  GetAInfo SizesA               = AInfo "sizes" '[Link, Img, Source]
  GetAInfo SlotA                = AInfo "slot" '[]
  GetAInfo SpanA                = AInfo "span" '[Col, Colgroup]
  GetAInfo SpellcheckA          = AInfo "spellcheck" '[]
  GetAInfo SrcA                 = AInfo "src" '[Audio, Embed, Iframe, Img, Input, Script, Source, Track, Video]
  GetAInfo SrcdocA              = AInfo "srcdoc" '[Iframe]
  GetAInfo SrclangA             = AInfo "srclang" '[Track]
  GetAInfo SrcsetA              = AInfo "srcset" '[Img]
  GetAInfo StartA               = AInfo "start" '[Ol]
  GetAInfo StepA                = AInfo "step" '[Input]
  GetAInfo StyleA               = AInfo "style" '[]
  GetAInfo SummaryA             = AInfo "summary" '[Table]
  GetAInfo TabindexA            = AInfo "tabindex" '[]
  GetAInfo TargetA              = AInfo "target" '[A, Area, Base, Form]
  GetAInfo TitleA               = AInfo "title" '[]
  GetAInfo TranslateA           = AInfo "translate" '[]
  GetAInfo TypeA                = AInfo "type" '[Button, Input, Command, Embed, Object, Script, Source, Style, Menu, Ol, A, Area, Link]
  GetAInfo TypemustmatchA       = AInfo "typemustmatch" '[Object]
  GetAInfo UsemapA              = AInfo "usemap" '[Img, Input, Object]
  GetAInfo ValueA               = AInfo "value" '[Button, Option, Input, Li, Meter, Progress, Param, Data]
  GetAInfo WidthA               = AInfo "width" '[Canvas, Embed, Iframe, Img, Input, Object, Video]
  GetAInfo WrapA                = AInfo "wrap" '[Textarea]

-- | Retrieve type level meta data about elements.
type family GetEInfo a = r | r -> a where

  GetEInfo DOCTYPE = EInfo
    "!DOCTYPE html"
    '[]
    NoContent

  GetEInfo A = EInfo
    "a"
    '[ FlowContent, PhrasingContent ]
    (FlowContent :&: NOT (SingleElement Details) :|: PhrasingContent)

  GetEInfo Abbr = EInfo
    "abbr"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Address = EInfo
    "address"
    '[ FlowContent ]
    (FlowContent :&: NOT (HeadingContent :|: SectioningContent :|: SingleElement Address :|: SingleElement Header :|: SingleElement Footer))

  GetEInfo Area = EInfo
    "area"
    '[ FlowContent, PhrasingContent ]
    NoContent

  GetEInfo Article = EInfo
    "article"
    '[ FlowContent, SectioningContent ]
    FlowContent

  GetEInfo Aside = EInfo
    "aside"
    '[ FlowContent, SectioningContent ]
    FlowContent

  GetEInfo Audio = EInfo
    "audio"
    '[ FlowContent, PhrasingContent ]
    (SingleElement Source :|: SingleElement Track)

  GetEInfo B = EInfo
    "b"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Base = EInfo
    "base"
    '[ MetadataContent ]
    NoContent

  GetEInfo Bdi = EInfo
    "bdi"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Bdo = EInfo
    "bdo"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Blockquote = EInfo
    "blockquote"
    '[ FlowContent ]
    FlowContent

  GetEInfo Body = EInfo
    "body"
    '[]
    FlowContent

  GetEInfo Br = EInfo
    "br"
    '[ FlowContent, PhrasingContent ]
    NoContent

  GetEInfo Button = EInfo
    "button"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Canvas = EInfo
    "canvas"
    '[ FlowContent, PhrasingContent ]
    (SingleElement A :|: SingleElement Button :|: SingleElement Input)

  GetEInfo Caption = EInfo
    "caption"
    '[]
    FlowContent

  GetEInfo Cite = EInfo
    "cite"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Code = EInfo
    "code"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Col = EInfo
    "col"
    '[]
    NoContent

  GetEInfo Colgroup = EInfo
    "colgroup"
    '[]
    (SingleElement Col)

  GetEInfo Data = EInfo
    "data"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Datalist = EInfo
    "datalist"
    '[ FlowContent, PhrasingContent ]
    (PhrasingContent :|: SingleElement Option)

  GetEInfo Dd = EInfo
    "dd"
    '[]
    FlowContent

  GetEInfo Del = EInfo
    "del"
    '[ FlowContent, PhrasingContent ]
    OnlyText

  GetEInfo Details = EInfo
    "details"
    '[ FlowContent ]
    ( FlowContent :|: SingleElement Summary)

  GetEInfo Dfn = EInfo
    "dfn"
    '[ FlowContent, PhrasingContent ]
    (PhrasingContent :&: NOT (SingleElement Dfn))

  GetEInfo Dialog = EInfo
    "dialog"
    '[ FlowContent ]
    FlowContent

  GetEInfo Div = EInfo
    "div"
    '[ FlowContent ]
    (FlowContent :|: SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template)

  GetEInfo Dl = EInfo
    "dl"
    '[ FlowContent ]
    (SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template :|: SingleElement Div)

  GetEInfo Dt = EInfo
    "dt"
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))

  GetEInfo Em = EInfo
    "em"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Embed = EInfo
    "embed"
    '[ FlowContent, PhrasingContent ]
    NoContent

  GetEInfo Fieldset = EInfo
    "fieldset"
    '[ FlowContent ]
    (FlowContent :|: SingleElement Legend)

  GetEInfo Figcaption = EInfo
    "figcaption"
    '[]
    FlowContent

  GetEInfo Figure = EInfo
    "figure"
    '[ FlowContent ]
    (FlowContent :|: SingleElement Figcaption)

  GetEInfo Footer = EInfo
    "footer"
    '[ FlowContent ]
    (FlowContent :&: NOT (SingleElement Footer :|: SingleElement Header))

  GetEInfo Form = EInfo
    "form"
    '[ FlowContent ]
    (FlowContent :&: NOT (SingleElement Form))

  GetEInfo H1 = EInfo
    "h1"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo H2 = EInfo
    "h2"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo H3 = EInfo
    "h3"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo H4 = EInfo
    "h4"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo H5 = EInfo
    "h5"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo H6 = EInfo
    "h6"
    '[ FlowContent, HeadingContent ]
    PhrasingContent

  GetEInfo Head = EInfo
    "head"
    '[]
    MetadataContent

  GetEInfo Header = EInfo
    "header"
    '[ FlowContent ]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer))

  GetEInfo Hgroup = EInfo
    "hgroup"
    '[ FlowContent, HeadingContent ]
    (SingleElement H1 :|: SingleElement H2 :|: SingleElement H3 :|: SingleElement H4 :|: SingleElement H5 :|: SingleElement H6)

  GetEInfo Hr = EInfo
    "hr"
    '[ FlowContent ]
    NoContent

  GetEInfo Html = EInfo
    "html"
    '[]
    (SingleElement Head :|: SingleElement Body)

  GetEInfo I = EInfo
    "i"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Iframe = EInfo
    "iframe"
    '[ FlowContent, PhrasingContent ]
    NoContent

  GetEInfo Img = EInfo
    "img"
    '[ FlowContent, PhrasingContent ]
    NoContent

  GetEInfo Ins = EInfo
    "ins"
    '[ FlowContent, PhrasingContent ]
    OnlyText

  GetEInfo Kbd = EInfo
    "kbd"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Label = EInfo
    "label"
    '[ FlowContent, PhrasingContent ]
    (PhrasingContent :&: NOT (SingleElement Label))

  GetEInfo Legend = EInfo
    "legend"
    '[]
    PhrasingContent

  GetEInfo Li = EInfo
    "li"
    '[]
    FlowContent

  GetEInfo Link = EInfo
    "link"
    '[ FlowContent, PhrasingContent, MetadataContent ]
    NoContent

  GetEInfo Main = EInfo
    "main"
    '[ FlowContent ]
    FlowContent

  GetEInfo Map = EInfo
    "map"
    '[ FlowContent, PhrasingContent ]
    OnlyText

  GetEInfo Mark = EInfo
    "mark"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Menu = EInfo
    "menu"
    '[ FlowContent ]
    (FlowContent :|: SingleElement Li :|: SingleElement Script :|: SingleElement Template :|: SingleElement Menu :|: SingleElement Menuitem :|: SingleElement Hr)

  GetEInfo Menuitem = EInfo
    "menuitem"
    '[]
    NoContent

  GetEInfo Meta = EInfo
    "meta"
    '[ FlowContent, MetadataContent, PhrasingContent ]
    NoContent

  GetEInfo Meter = EInfo
    "meter"
    '[ FlowContent, PhrasingContent ]
    (PhrasingContent :&: NOT (SingleElement Meter))

  GetEInfo Nav = EInfo
    "nav"
    '[ FlowContent, SectioningContent ]
    FlowContent

  GetEInfo Noscript = EInfo
    "noscript"
    '[ FlowContent, MetadataContent, PhrasingContent ]
    (FlowContent :|: PhrasingContent :|: SingleElement Link :|: SingleElement Style :|: SingleElement Meta)

  GetEInfo Object = EInfo
    "object"
    '[ FlowContent, PhrasingContent ]
    (SingleElement Param)

  GetEInfo Ol = EInfo
    "ol"
    '[ FlowContent ]
    (SingleElement Li)

  GetEInfo Optgroup = EInfo
    "optgroup"
    '[]
    (SingleElement Option)

  GetEInfo Option = EInfo
    "option"
    '[]
    OnlyText

  GetEInfo Output = EInfo
    "output"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo P = EInfo
    "p"
    '[ FlowContent ]
    PhrasingContent

  GetEInfo Param = EInfo
    "param"
    '[]
    NoContent

  GetEInfo Picture = EInfo
    "picture"
    '[ FlowContent, PhrasingContent ]
    (SingleElement Source :|: SingleElement Img)

  GetEInfo Pre = EInfo
    "pre"
    '[ FlowContent ]
    PhrasingContent

  GetEInfo Progress = EInfo
    "progress"
    '[ FlowContent, PhrasingContent ]
    (PhrasingContent :&: NOT (SingleElement Progress))

  GetEInfo Q = EInfo
    "q"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Rp = EInfo
    "rp"
    '[]
    OnlyText

  GetEInfo Rt = EInfo
    "rt"
    '[]
    PhrasingContent

  GetEInfo Rtc = EInfo
    "rtc"
    '[]
    (PhrasingContent :|: SingleElement Rt)

  GetEInfo Ruby = EInfo
    "ruby"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo S = EInfo
    "s"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Samp = EInfo
    "samp"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Script = EInfo
    "script"
    '[ FlowContent, MetadataContent, PhrasingContent ]
    OnlyText

  GetEInfo Section = EInfo
    "section"
    '[ FlowContent, SectioningContent ]
    FlowContent

  GetEInfo Select = EInfo
    "select"
    '[ FlowContent, PhrasingContent ]
    (SingleElement Option :|: SingleElement Optgroup)

  GetEInfo Slot = EInfo
    "slot"
    '[ FlowContent, PhrasingContent ]
    OnlyText

  GetEInfo Small = EInfo
    "small"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Source = EInfo
    "source"
    '[]
    NoContent

  GetEInfo Span = EInfo
    "span"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Strong = EInfo
    "strong"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Style = EInfo
    "style"
    '[ FlowContent, MetadataContent ]
    OnlyText

  GetEInfo Sub = EInfo
    "sub"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Summary = EInfo
    "summary"
    '[]
    (PhrasingContent :|: HeadingContent)

  GetEInfo Sup = EInfo
    "sup"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Table = EInfo
    "table"
    '[FlowContent]
    (SingleElement Caption :|: SingleElement Colgroup :|: SingleElement Thead :|: SingleElement Tbody :|: SingleElement Tr :|: SingleElement Tfoot)

  GetEInfo Tbody = EInfo
    "tbody"
    '[]
    (SingleElement Tr)

  GetEInfo Td = EInfo
    "td"
    '[]
    FlowContent

  GetEInfo Template = EInfo
    "template"
    '[ FlowContent, MetadataContent, PhrasingContent ]
    (FlowContent :|: MetadataContent)

  GetEInfo Textarea = EInfo
    "textarea"
    '[ FlowContent, PhrasingContent ]
    OnlyText

  GetEInfo Tfoot = EInfo
    "tfoot"
    '[]
    (SingleElement Tr)

  GetEInfo Th = EInfo
    "th"
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))

  GetEInfo Thead = EInfo
    "thead"
    '[]
    (SingleElement Tr)

  GetEInfo Time = EInfo
    "time"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Title = EInfo
    "title"
    '[ MetadataContent ]
    OnlyText

  GetEInfo Tr = EInfo
    "tr"
    '[]
    (SingleElement Td :|: SingleElement Th)

  GetEInfo Track = EInfo
    "track"
    '[]
    NoContent

  GetEInfo U = EInfo
    "u"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Ul = EInfo
    "ul"
    '[ FlowContent ]
    (SingleElement Li)

  GetEInfo Var = EInfo
    "var"
    '[ FlowContent, PhrasingContent ]
    PhrasingContent

  GetEInfo Video = EInfo
    "video"
    '[ FlowContent, PhrasingContent ]
    (SingleElement Track :|: SingleElement Source)

  GetEInfo Wbr = EInfo
    "wbr"
    '[ FlowContent, PhrasingContent ]
    NoContent
