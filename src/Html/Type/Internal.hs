{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE GADTs                  #-}

{-# LANGUAGE CPP #-}

module Html.Type.Internal where

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.Type.Bool
import Data.ByteString (ByteString)

-- | Data for declaring variables in a html document which will be compacted.
data V (n :: Symbol) = V

newtype One a = One a

-- | Unique set of variables in a html document in the order of occurence.
type Variables a = Dedupe (GetV a)

-- | A compacted html documented with it's variables annoted as a list of Symbols.
data CompactHTML (a :: [Symbol]) = MkCompactHTML ByteString [(Int, ByteString)] deriving Show

type family GetV a :: [Symbol] where
  GetV (a # b)       = Append (GetV a) (GetV b)
  GetV ((a :@: b) c) = Append (GetV b) (GetV c)
  GetV (a := b)      = GetV b
  GetV (Maybe a)     = GetV a
  GetV [a]           = GetV a
  GetV (Either a b)  = Append (GetV a) (GetV b)
  GetV (V v)         = '[v]
  GetV x             = '[]

type family Reverse xs where
  Reverse xs = Reverse' xs '[]

type family Reverse' xs ys where
  Reverse' (x':xs) ys = Reverse' xs (x':ys)
  Reverse' '[] ys = ys

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
  showTypeList = symbolVal (Proxy @ x) : showTypeList @ xs

instance ShowTypeList '[] where
  showTypeList = []

type GetElementName         (e :: Element name categories contentModel contentAttributes) = name
type GetElementCategories   (e :: Element name categories contentModel contentAttributes) = categories
type GetElementContentModel (e :: Element name categories contentModel contentAttributes) = contentModel
type GetElementAttributes   (e :: Element name categories contentModel contentAttributes) = contentAttributes

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

-- | \ 3.2.6 Global attributes
type GlobalAttributes =
  [ AccesskeyA
  , AutocapitalizeA
  , AutofocusA
  , ContenteditableA
  , DirA
  , DraggableA
  , EnterkeyhintA
  , HiddenA
  , InputmodeA
  , IsA
  , ItemidA
  , ItempropA
  , ItemrefA
  , ItemscopeA
  , ItemtypeA
  , LangA
  , NonceA
  , SpellcheckA
  , StyleA
  , TabindexA
  , TitleA
  , TranslateA
  -- user agent requirements
  , ClassA
  , IdA
  , SlotA
  -- event handler content attributes
  , OnabortA
  , OnauxclickA
  , OnblurA
  , OncancelA
  , OncanplayA
  , OncanplaythroughA
  , OnchangeA
  , OnclickA
  , OncloseA
  , OncontextmenuA
  , OncopyA
  , OncuechangeA
  , OncutA
  , OndblclickA
  , OndragA
  , OndragendA
  , OndragenterA
  , OndragleaveA
  , OndragoverA
  , OndragstartA
  , OndropA
  , OndurationchangeA
  , OnemptiedA
  , OnendedA
  , OnerrorA
  , OnfocusA
  , OnformdataA
  , OninputA
  , OninvalidA
  , OnkeydownA
  , OnkeypressA
  , OnkeyupA
  , OnloadA
  , OnloadeddataA
  , OnloadedmetadataA
  , OnloadstartA
  , OnmousedownA
  , OnmouseenterA
  , OnmouseleaveA
  , OnmousemoveA
  , OnmouseoutA
  , OnmouseoverA
  , OnmouseupA
  , OnpasteA
  , OnpauseA
  , OnplayA
  , OnplayingA
  , OnprogressA
  , OnratechangeA
  , OnresetA
  , OnresizeA
  , OnscrollA
  , OnsecuritypolicyviolationA
  , OnseekedA
  , OnseekingA
  , OnselectA
  , OnslotchangeA
  , OnstalledA
  , OnsubmitA
  , OnsuspendA
  , OntimeupdateA
  , OntoggleA
  , OnvolumechangeA
  , OnwaitingA
  , OnwheelA

  -- ... aria ?
  -- ... data ?

  ]

-- | 4 The elements of HTML
data Element (name :: Symbol) (categories :: [ContentCategory]) (contentModel :: ContentCategory) (contentAttributes :: [Attribute]) where

  DOCTYPE
    :: Element
    "!DOCTYPE html"
    '[]
    None
    '[]

  -- | \ 4.1 The document element
  --     4.1.1
  Html
    :: Element
    "html"
    '[]
    -- A head element followed by a body element.
    (Elements ["head", "body"])
    '[ManifestA]

  -- | \ 4.2 Document metadata
  --     4.2.1
  Head
    :: Element
    "head"
    '[]
    -- If the document is an iframe srcdoc document or if title
    -- information is available from a higher-level protocol: Zero or
    -- more elements of metadata content, of which no more than one is
    -- a title element and no more than one is a base
    -- element. Otherwise: One or more elements of metadata content,
    -- of which exactly one is a title element and no more than one is
    -- a base element.
    Metadata
    '[]

  -- | \ 4.2.2
  Title
    :: Element
    "title"
    '[Metadata]
    -- Text that is not inter-element whitespace.
    OnlyText
    '[]

  -- | \ 4.2.3
  Base
    :: Element
    "base"
    '[Metadata]
    None
    '[HrefA, TargetA]

  -- | \ 4.2.4
  Link
    :: Element
    "link"
    '[Metadata, Flow, Phrasing]
    None
    '[HrefA, CrossoriginA, RelA, MediaA, IntegrityA, HreflangA, TypeA, ReferrerpolicyA, SizesA, ImagesrcsetA, ImagesizesA, AsA, RelA, ColorA, DisabledA]

  -- | \ 4.2.5
  Meta
    :: Element
    "meta"
    '[Metadata, Flow, Phrasing]
    None
    '[NameA, HttpEquivA, ContentA, CharsetA]

  -- | \ 4.2.6
  Style
    :: Element
    "style"
    '[Metadata]
    -- Text that gives a conformant style sheet.
    OnlyText
    '[MediaA]

  -- | \ 4.3 Sections
  --     4.3.1
  Body
    :: Element
    "body"
    '[]
    Flow
    '[OnafterprintA, OnbeforeprintA, OnbeforeunloadA, OnhashchangeA, OnlanguagechangeA, OnmessageA, OnmessageerrorA, OnofflineA, OnonlineA, OnpagehideA, OnpageshowA, OnpopstateA, OnrejectionhandledA, OnstorageA, OnunhandledrefectionA, OnunloadA]

  -- | \ 4.3.2
  Article
    :: Element
    "article"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

  -- | \ 4.3.3
  Section
    :: Element
    "section"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

  -- | \ 4.3.4
  Nav
    :: Element
    "nav"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

  -- | \ 4.3.5
  Aside
    :: Element
    "aside"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

  -- | \ 4.3.6
  H1
    :: Element
    "h1"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  H2
    :: Element
    "h2"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  H3
    :: Element
    "h3"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  H4
    :: Element
    "h4"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  H5
    :: Element
    "h5"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  H6
    :: Element
    "h6"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

  -- | \ 4.3.7
  Hgroup
    :: Element
    "hgroup"
    '[Flow, Heading, Palpable]
    ((Heading :&: NOT (Elements '["hgroup"])) :|: Scripting)
    '[]

  -- | \ 4.3.8
  Header
    :: Element
    "header"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements ["header", "footer"]))
    '[]

  -- | \ 4.3.9
  Footer
    :: Element
    "footer"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements ["header", "footer"]))
    '[]

  -- | \ 4.3.10
  Address
    :: Element
    "address"
    '[Flow, Palpable]
    -- Flow content, but with no heading content descendants, no
    -- sectioning content descendants, and no header, footer, or
    -- address element descendants.
    (Flow :&: NOT (Heading :|: Sectioning :|: Elements ["header", "footer", "address"]))
    '[]

  -- | \ 4.4 Grouping content
  --     4.4.1
  P
    :: Element
    "p"
    '[Flow, Palpable]
    Phrasing
    '[]

  -- | \ 4.4.2
  Hr
    :: Element
    "hr"
    '[Flow]
    None
    '[]

  -- | \ 4.4.3
  Pre
    :: Element
    "pre"
    '[Flow, Palpable]
    Phrasing
    '[]

  -- | \ 4.4.4
  Blockquote
    :: Element
    "blockquote"
    '[Flow, Palpable]
    Flow
    '[CiteA]

  -- | \ 4.4.5
  Ol
    :: Element
    "ol"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[ReversedA, StartA, TypeA]

  -- | \ 4.4.6
  Ul
    :: Element
    "ul"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]

  -- | \ 4.4.7
  Menu
    :: Element
    "menu"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]

  -- | \ 4.4.8
  Li
    :: Element
    "li"
    '[]
    Flow
    '[ValueA]

  -- | \ 4.4.9
  Dl
    :: Element
    "dl"
    '[Flow, Palpable]
    (Elements ["dt", "dd", "div"] :|: Scripting)
    '[]

  -- | \ 4.4.10
  Dt
    :: Element
    "dt"
    '[]
    (Flow :&: NOT (Sectioning :|: Heading :|: Elements ["header", "footer"]))
    '[]

  -- | \ 4.4.11
  Dd
    :: Element
    "dd"
    '[]
    Flow
    '[]

  -- | \ 4.4.12
  Figure
    :: Element
    "figure"
    '[Flow, Palpable]
    -- Either: one figcaption element followed by flow content. Or:
    -- flow content followed by one figcaption element. Or: flow
    -- content.
    (Flow :|: Elements '["figcaption"])
    '[]

  -- | \ 4.4.13
  Figcaption
    :: Element
    "figcaption"
    '[]
    Flow
    '[]

  -- | \ 4.4.14
  Main
    :: Element
    "main"
    '[Flow, Palpable]
    Flow
    '[]

  -- | \ 4.4.15
  Div
    :: Element
    "div"
    '[Flow, Palpable]
    -- If the element is a child of a dl element: one or more dt
    -- elements followed by one or more dd elements, optionally
    -- intermixed with script-supporting elements. If the element is
    -- not a child of a dl element: flow content.
    (Flow :|: Elements ["dt", "dt"] :|: Scripting)
    '[]

  -- | \ 4.5 Text-level semantics
  --     4.5.1
  A
    :: Element
    "a"
    '[Flow, Phrasing, Interactive, Palpable]
    -- Transparent, but there must be no interactive content
    -- descendant, a element descendant, or descendant with the
    -- tabindex attribute specified.
    ((Flow :|: Phrasing :|: Palpable) :&: NOT (Elements '["a"]))
    '[HrefA, TargetA, DownloadA, PingA, RelA, HreflangA, TypeA, ReferrerpolicyA]

  -- | \ 4.5.2
  Em
    :: Element
    "em"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.3
  Strong
    :: Element
    "strong"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.4
  Small
    :: Element
    "small"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.5
  S
    :: Element
    "s"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.6
  Cite
    :: Element
    "cite"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.7
  Q
    :: Element
    "q"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[CiteA]

  -- | \ 4.5.8
  Dfn
    :: Element
    "dfn"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["dfn"]))
    '[]

  -- | \ 4.5.9
  Abbr
    :: Element
    "abbr"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.10
  Ruby
    :: Element
    "ruby"
    '[Flow, Phrasing, Palpable]
    (Phrasing :|: Elements ["rt", "rp"])
    '[]

  -- | \ 4.5.11
  Rt
    :: Element
    "rt"
    '[]
    Phrasing
    '[]

  -- | \ 4.5.12
  Rp
    :: Element
    "rp"
    '[]
    OnlyText
    '[]

  -- | \ 4.5.13
  Data
    :: Element
    "data"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[ValueA]

  -- | \ 4.5.14
  Time
    :: Element
    "time"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[DatetimeA]

  -- | \ 4.5.15
  Code
    :: Element
    "code"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.16
  Var
    :: Element
    "var"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.17
  Samp
    :: Element
    "samp"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.18
  Kbd
    :: Element
    "kbd"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.19
  Sub
    :: Element
    "sub"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  Sup
    :: Element
    "sup"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.20
  I
    :: Element
    "i"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.21
  B
    :: Element
    "b"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.22
  U
    :: Element
    "u"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.23
  Mark
    :: Element
    "mark"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.24
  Bdi
    :: Element
    "bdi"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.25
  Bdo
    :: Element
    "bdo"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.26
  Span
    :: Element
    "span"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

  -- | \ 4.5.27
  Br
    :: Element
    "br"
    '[Flow, Phrasing]
    None
    '[]

  -- | \ 4.5.28
  Wbr
    :: Element
    "wbr"
    '[Flow, Phrasing]
    None
    '[]

  -- | \ 4.7 Edits
  --     4.7.1
  Ins
    :: Element
    "ins"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '[CiteA, DatetimeA]

  -- | \ 4.7.2
  Del
    :: Element
    "del"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '[CiteA, DatetimeA]

  -- | \ 4.8 Embedded content
  --     4.8.1
  Picture
    :: Element
    "picture"
    '[Flow, Phrasing, Embedded]
    (Elements ["source", "img"] :|: Scripting)
    '[]

  -- | \ 4.8.2
  Source
    :: Element
    "source"
    '[]
    None
    '[SrcA, TypeA, SrcsetA, SizesA, MediaA]

  -- | \ 4.8.3
  Img
    :: Element
    "img"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '[AltA, SrcA, SrcsetA, SizesA, CrossoriginA, UsemapA, IsmapA, WidthA, HeightA, ReferrerpolicyA, DecodingA, LoadingA]

  -- | \ 4.8.5
  Iframe
    :: Element
    "iframe"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '[SrcA, SrcdocA, NameA, SandboxA, AllowA, AllowfullscreenA, WidthA, HeightA, ReferrerpolicyA, LoadingA]

  -- | \ 4.8.6
  Embed
    :: Element
    "embed"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '[SrcA, TypeA, WidthA, HeightA]

  -- | \ 4.8.7
  Object
    :: Element
    "object"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    (Elements '["param"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable)
    '[DataA, TypeA, NameA, UsemapA, FormA, WidthA, HeightA]

  -- | \ 4.8.8
  Param
    :: Element
    "param"
    '[]
    None
    '[NameA, ValueA]

  -- | \ 4.8.9
  Video
    :: Element
    "video"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements ["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements ["audio", "video"]))
    '[SrcA, CrossoriginA, PosterA, PreloadA, AutoplayA, PlaysinlineA, LoopA, MutedA, ControlsA, WidthA, HeightA]

  -- | \ 4.8.10
  Audio
    :: Element
    "audio"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements ["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements ["audio", "video"]))
    '[SrcA, CrossoriginA, PreloadA, AutoplayA, LoopA, MutedA, ControlsA]

  -- | \ 4.8.11
  Track
    :: Element
    "track"
    '[]
    None
    '[KindA, SrcA, SrclangA, LabelA, DefaultA]

  -- | \ 4.8.13
  Map
    :: Element
    "map"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '[NameA]

  -- | \ 4.8.14
  Area
    :: Element
    "area"
    '[Flow, Phrasing]
    None
    '[AltA, CoordsA, ShapeA, HrefA, TargetA, DownloadA, PingA, RelA, ReferrerpolicyA]

  -- | \ 4.9 Tabular data
  --     4.9.1
  Table
    :: Element
    "table"
    '[Flow, Palpable]
    (Elements ["caption", "colgroup", "thead", "tbody", "tr", "tfoot"] :|: Scripting)
    '[]

  -- | \ 4.9.2
  Caption
    :: Element
    "caption"
    '[]
    (Flow :|: NOT (Elements '["table"]))
    '[]

  -- | \ 4.9.3
  Colgroup
    :: Element
    "colgroup"
    '[]
    (Elements ["col", "template"])
    '[SpanA]

  -- | \ 4.9.4
  Col
    :: Element
    "col"
    '[]
    None
    '[SpanA]

  -- | \ 4.9.5
  Tbody
    :: Element
    "tbody"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

  -- | \ 4.9.6
  Thead
    :: Element
    "thead"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

  -- | \ 4.9.7
  Tfoot
    :: Element
    "tfoot"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

  -- | \ 4.9.8
  Tr
    :: Element
    "tr"
    '[]
    (Elements ["td", "th"] :|: Scripting)
    '[]

  -- | \ 4.9.9
  Td
    :: Element
    "td"
    '[]
    Flow
    '[ColspanA, RowspanA, HeadersA]

  -- | \ 4.9.10
  Th
    :: Element
    "th"
    '[]
    (Flow :&: NOT (Elements ["header", "footer"] :|: Sectioning :|: Heading))
    '[ColspanA, RowspanA, HeadersA, ScopeA, AbbrA]

  -- | \ 4.10 Forms
  --     4.10.3
  Form
    :: Element
    "form"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["form"]))
    '[AcceptCharsetA, ActionA, AutocompleteA, EnctypeA, MethodA, NameA, NovalidateA, TargetA, RelA]

  -- | \ 4.10.4
  Label
    :: Element
    "label"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT (Elements '["label"]))
    '[ForA]

  -- | \ 4.10.5
  Input
    :: Element
    "input"
    '[Flow, Phrasing, Interactive, Palpable]
    None
    '[AcceptA, AltA, AutocompleteA, CheckedA, DirnameA, DisabledA, FormA, FormactionA, FormenctypeA, FormmethodA, FormnovalidateA, FormtargetA, HeightA, ListA, MaxA, MaxlengthA, MinA, MinlengthA, MultipleA, NameA, PatternA, PlaceholderA, ReadonlyA, RequiredA, SizeA, SrcA, StepA, TypeA, ValueA, WidthA]

  -- | \ 4.10.6
  Button
    :: Element
    "button"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT Interactive)
    '[DisabledA, FormA, FormactionA, FormenctypeA, FormmethodA, FormnovalidateA, FormtargetA, NameA, TypeA, ValueA]

  -- | \ 4.10.7
  Select
    :: Element
    "select"
    '[Flow, Phrasing, Interactive, Palpable]
    (Elements ["option", "optgroup"] :|: Scripting)
    '[AutocompleteA, DisabledA, FormA, MultipleA, NameA, RequiredA, SizeA]

  -- | \ 4.10.8
  Datalist
    :: Element
    "datalist"
    '[Flow, Phrasing]
    (Phrasing :|: Scripting :|: Elements '["option"])
    '[]

  -- | \ 4.10.9
  Optgroup
    :: Element
    "optgroup"
    '[]
    (Elements '["option"] :|: Scripting)
    '[DisabledA, LabelA]

  -- | \ 4.10.10
  Option
    :: Element
    "option"
    '[]
    OnlyText
    '[DisabledA, LabelA, SelectedA, ValueA]

  -- | \ 4.10.11
  Textarea
    :: Element
    "textarea"
    '[Flow, Phrasing, Interactive, Palpable]
    OnlyText
    '[AutocompleteA, ColsA, DirnameA, DisabledA, FormA, MaxlengthA, MinlengthA, NameA, PlaceholderA, ReadonlyA, RequiredA, RowsA, WrapA]

  -- | \ 4.10.12
  Output
    :: Element
    "output"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[ForA, FormA, NameA]

  -- | \ 4.10.13
  Progress
    :: Element
    "progress"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["progress"]))
    '[ValueA, MaxA]

  -- | \ 4.10.14
  Meter
    :: Element
    "meter"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["meter"]))
    '[ValueA, MinA, MaxA, LowA, HighA, OptimumA]

  -- | \ 4.10.15
  Fieldset
    :: Element
    "fieldset"
    '[Flow, Palpable]
    (Elements '["legend"] :|: Flow)
    '[DisabledA, FormA, NameA]

  -- | \ 4.10.16
  Legend
    :: Element
    "legend"
    '[]
    (Phrasing :|: Heading)
    '[]

  -- | \ 4.11 Interactive elements
  --     4.11.1
  Details
    :: Element
    "details"
    '[Flow, Interactive, Palpable]
    (Elements '["summary"] :|: Flow)
    '[OpenA]

  -- | \ 4.11.2
  Summary
    :: Element
    "summary"
    '[]
    (Phrasing :|: Heading)
    '[]

  -- | \ 4.11.4
  Dialog
    :: Element
    "dialog"
    '[Flow]
    Flow
    '[OpenA]

  -- | \ 4.12 Scripting
  --     4.12.1
  Script
    :: Element
    "script"
    '[Metadata, Flow, Phrasing, Scripting]
    OnlyText
    '[SrcA, TypeA, NomoduleA, AsyncA, DeferA, CrossoriginA, IntegrityA, ReferrerpolicyA]

  -- | \ 4.12.2
  Noscript
    :: Element
    "noscript"
    '[Metadata, Flow, Phrasing]
    ((Elements ["link", "style", "meta"] :|: Metadata :|: Flow :|: Phrasing) :&: NOT (Elements '["noscript"]))
    '[]

  -- | \ 4.12.3
  Template
    :: Element
    "template"
    '[Metadata, Flow, Phrasing, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Palpable)
    '[]

  -- | \ 4.12.4
  Slot
    :: Element
    "slot"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '[NameA]

  -- | \ 4.12.5
  Canvas
    :: Element
    "canvas"
    '[Flow, Phrasing, Embedded, Palpable]
    (((Flow :|: Phrasing :|: Embedded :|: Palpable) :&: NOT Interactive) :|: Elements ["a", "img", "button", "input", "select"])
    '[WidthA, HeightA]

  -- | \ 4.13 Custom elements
  CustomElement
    :: Element
    name
    categories
    contentModel
    contentAttributes

-- | Index
--   Attributes
data Attribute
  = RoleA
  | AriaActivedescendantA
  | AriaAtomicA
  | AriaAutocompleteA
  | AriaBusyA
  | AriaCheckedA
  | AriaControlsA
  | AriaDescribedbyA
  | AriaDisabledA
  | AriaDropeffectA
  | AriaExpandedA
  | AriaFlowtoA
  | AriaGrabbedA
  | AriaHaspopupA
  | AriaHiddenA
  | AriaInvalidA
  | AriaLabelA
  | AriaLabelledByA
  | AriaLevelA
  | AriaLiveA
  | AriaMultilineA
  | AriaMultiselectableA
  | AriaOwnsA
  | AriaPosinsetA
  | AriaPressedA
  | AriaReadonlyA
  | AriaRelevantA
  | AriaRequiredA
  | AriaSelectedA
  | AriaSetsizeA
  | AriaSortA
  | AriaValuemaxA
  | AriaValueminA
  | AriaValuenowA
  | AriaValuetextA

  | AbbrA
  | AcceptA
  | AcceptCharsetA
  | AccesskeyA
  | ActionA
  | AllowA
  | AllowfullscreenA
  | AllowpaymentrequestA
  | AlignA
  | AltA
  | AsA
  | AsyncA
  | AutocapitalizeA
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
  | DecodingA
  | DefaultA
  | DeferA
  | DirA
  | DirnameA
  | DisabledA
  | DownloadA
  | DraggableA
  | DropzoneA
  | EnctypeA
  | EnterkeyhintA
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
  | ImagesrcsetA
  | ImagesizesA
  | InputmodeA
  | IntegrityA
  | IsA
  | IsmapA
  | ItemidA
  | ItempropA
  | ItemrefA
  | ItemscopeA
  | ItemtypeA
  | KeytypeA
  | KindA
  | LabelA
  | LangA
  | LanguageA
  | ListA
  | LoadingA
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
  | NomoduleA
  | NonceA
  | NovalidateA
  | OnafterprintA
  | OnbeforeprintA
  | OnbeforeunloadA
  | OnhashchangeA
  | OnlanguagechangeA
  | OnmessageA
  | OnmessageerrorA
  | OnofflineA
  | OnonlineA
  | OnpagehideA
  | OnpageshowA
  | OnpopstateA
  | OnrejectionhandledA
  | OnstorageA
  | OnunhandledrefectionA
  | OnunloadA
  | OpenA
  | OptimumA
  | PatternA
  | PingA
  | PlaceholderA
  | PlaysinlineA
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

  | OnabortA
  | OnauxclickA
  | OnblurA
  | OncancelA
  | OncanplayA
  | OncanplaythroughA
  | OnchangeA
  | OnclickA
  | OncloseA
  | OncontextmenuA
  | OncopyA
  | OncuechangeA
  | OncutA
  | OndblclickA
  | OndragA
  | OndragendA
  | OndragenterA
  | OndragleaveA
  | OndragoverA
  | OndragstartA
  | OndropA
  | OndurationchangeA
  | OnemptiedA
  | OnendedA
  | OnerrorA
  | OnfocusA
  | OnformdataA
  | OninputA
  | OninvalidA
  | OnkeydownA
  | OnkeypressA
  | OnkeyupA
  | OnloadA
  | OnloadeddataA
  | OnloadedmetadataA
  | OnloadstartA
  | OnmousedownA
  | OnmouseenterA
  | OnmouseleaveA
  | OnmousemoveA
  | OnmouseoutA
  | OnmouseoverA
  | OnmouseupA
  | OnpasteA
  | OnpauseA
  | OnplayA
  | OnplayingA
  | OnprogressA
  | OnratechangeA
  | OnresetA
  | OnresizeA
  | OnscrollA
  | OnsecuritypolicyviolationA
  | OnseekedA
  | OnseekingA
  | OnselectA
  | OnslotchangeA
  | OnstalledA
  | OnsubmitA
  | OnsuspendA
  | OntimeupdateA
  | OntoggleA
  | OnvolumechangeA
  | OnwaitingA
  | OnwheelA

  | CustomA Symbol

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

type OpenTag e = AppendSymbol "<" (AppendSymbol (GetElementName e) ">")

type CloseTag e = AppendSymbol "</" (AppendSymbol (GetElementName e) ">")

-- | Flatten a document into a type list of tags.
type family ToList a :: List where
  ToList (a # b)         = ToList a >< ToList b
  ToList ((a :@: ()) ()) = 'List '[] (If (HasContent (GetElementContentModel a)) (AppendSymbol (OpenTag a) (CloseTag a)) (OpenTag a))
  ToList ((a :@: b) ())  = AppendSymbol "<" (GetElementName a) <| ToList b |> If (HasContent (GetElementContentModel a)) (AppendSymbol ">" (CloseTag a)) ">"
  ToList ((a :@: ()) b)  = OpenTag a <| ToList b |> CloseTag a
  ToList ((a :@: b) c)   = (AppendSymbol "<" (GetElementName a) <| ToList b) >< (">" <| ToList c |> CloseTag a)
  ToList (a := ())       = 'List '[] (AppendSymbol " " (ShowAttribute a))
  ToList (a := b)        = AppendSymbol " " (AppendSymbol (ShowAttribute a) "=\"") <| ToList b |> "\""
  ToList ()              = 'List '[] ""
  ToList (Proxy x)       = 'List '[] x
  ToList x               = 'List '[""] ""

newtype (:=) (a :: Attribute) b = AT b

-- | Check whether `b` is a valid child of `a`.
type a ?> b = Check CheckElement a b

-- | Check whether `a` is a valid attribute and `b` is a valid child of `p`.
type (<?>) p a b = (Check CheckAttribute p a, Check CheckElement p b)

type TextE a = Text (GetElementName a)
type TextA a = Text (ShowAttribute a)
type TagE a = Text "<" :<>: TextE a :<>: Text ">"

data CheckData
  = CheckElement
  | CheckAttribute

type family Check f a b :: Constraint where
  Check _ _ ()                      = ()
  Check _ _ (Raw _)                 = ()
  Check f a (b # c)                 = (Check f a b, Check f a c)
  Check f a (Maybe b)               = Check f a b
  Check f a (Either b c)            = (Check f a b, Check f a c)
  Check f a (b -> c)                = TypeError (TagE a :<>: Text " can't contain a function.")
  Check CheckElement a ((b :@: _) _)      = MaybeTypeError a b (CheckContentCategory (GetElementName b) ((GetElementContentModel a)) (GetElementCategories b))
  Check CheckElement a (f ((b :@: c) d))  = Check CheckElement a ((b :@: c) d)
  Check CheckElement a (f (b # c))        = Check CheckElement a (b # c)
  Check CheckElement a (b := c)           = TypeError (TagE a :<>: Text " can't contain an attribute." :$$: Text "Try '" :<>: TextE a :<>: Text "_A' instead.")
  Check CheckElement a b                  = CheckString a b
  Check CheckAttribute a (CustomA z := _) = ()
  Check CheckAttribute a (b := _)         = If (Elem b (Append (GetElementAttributes a) GlobalAttributes))
                                        (() :: Constraint)
                                        (TypeError (TextA b :<>: Text " is not a valid attribute of " :<>: TagE a :<>: Text "."))
  Check CheckAttribute _ b                = TypeError (ShowType b :<>: Text " is not an attribute.")

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
infixr 6 >

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
data (:@:) (a :: Element name categories contentModel contentAttributes) b c where
  WithAttributes :: (a <?> b) c => b -> c -> (a :@: b) c
infixr 8 :@:

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw {fromRaw :: a}

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
  HasContent None = False
  HasContent _    = True

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

-- | Check whether a given element may contain a non document type.
type family CheckString a b where
  CheckString a b = If (CheckContentCategory "" (GetElementContentModel a) '[OnlyText, Flow, Phrasing])
                       (() :: Constraint)
                       (TypeError (TagE a :<>: Text " can't contain a " :<>: ShowType b))

infixr 2 :|:
infixr 3 :&:

type family MaybeTypeError a b c where
  MaybeTypeError a b c = If c (() :: Constraint)
   (TypeError (TagE b :<>: Text " is not a valid child of " :<>: TagE a :<>: Text "."))

type family Elem (a :: k) (xs :: [k]) where
  Elem a (a : xs) = True
  Elem a (_ : xs) = Elem a xs
  Elem a '[]      = False

newtype T (proxies :: k) target = T target

-- | Get type list of valid elements for a given attribute.  An empty list signifies global attribute.
type family ShowAttribute a where

  ShowAttribute RoleA                 = "role"
  ShowAttribute AriaActivedescendantA = "aria-activedescendant"
  ShowAttribute AriaAtomicA           = "aria-atomic"
  ShowAttribute AriaAutocompleteA     = "aria-autocomplete"
  ShowAttribute AriaBusyA             = "aria-busy"
  ShowAttribute AriaCheckedA          = "aria-checked"
  ShowAttribute AriaControlsA         = "aria-controls"
  ShowAttribute AriaDescribedbyA      = "aria-describedby"
  ShowAttribute AriaDisabledA         = "aria-disabled"
  ShowAttribute AriaDropeffectA       = "aria-dropeffect"
  ShowAttribute AriaExpandedA         = "aria-expanded"
  ShowAttribute AriaFlowtoA           = "aria-flowto"
  ShowAttribute AriaGrabbedA          = "aria-grabbed"
  ShowAttribute AriaHaspopupA         = "aria-haspopup"
  ShowAttribute AriaHiddenA           = "aria-hidden"
  ShowAttribute AriaInvalidA          = "aria-invalid"
  ShowAttribute AriaLabelA            = "aria-label"
  ShowAttribute AriaLabelledByA       = "aria-labelledBy"
  ShowAttribute AriaLevelA            = "aria-level"
  ShowAttribute AriaLiveA             = "aria-live"
  ShowAttribute AriaMultilineA        = "aria-multiline"
  ShowAttribute AriaMultiselectableA  = "aria-multiselectable"
  ShowAttribute AriaOwnsA             = "aria-owns"
  ShowAttribute AriaPosinsetA         = "aria-posinset"
  ShowAttribute AriaPressedA          = "aria-pressed"
  ShowAttribute AriaReadonlyA         = "aria-readonly"
  ShowAttribute AriaRelevantA         = "aria-relevant"
  ShowAttribute AriaRequiredA         = "aria-required"
  ShowAttribute AriaSelectedA         = "aria-selected"
  ShowAttribute AriaSetsizeA          = "aria-setsize"
  ShowAttribute AriaSortA             = "aria-sort"
  ShowAttribute AriaValuemaxA         = "aria-valuemax"
  ShowAttribute AriaValueminA         = "aria-valuemin"
  ShowAttribute AriaValuenowA         = "aria-valuenow"
  ShowAttribute AriaValuetextA        = "aria-valuetext"

  ShowAttribute AcceptA               = "accept"
  ShowAttribute AcceptCharsetA        = "accept-charset"
  ShowAttribute AccesskeyA            = "accesskey"
  ShowAttribute ActionA               = "action"
  ShowAttribute AllowfullscreenA      = "allowfullscreen"
  ShowAttribute AllowpaymentrequestA  = "allowpaymentrequest"
  ShowAttribute AlignA                = "align"
  ShowAttribute AltA                  = "alt"
  ShowAttribute AsyncA                = "async"
  ShowAttribute AutocompleteA         = "autocomplete"
  ShowAttribute AutofocusA            = "autofocus"
  ShowAttribute AutoplayA             = "autoplay"
  ShowAttribute AutosaveA             = "autosave"
  ShowAttribute BgcolorA              = "bgcolor"
  ShowAttribute BorderA               = "border"
  ShowAttribute BufferedA             = "buffered"
  ShowAttribute ChallengeA            = "challenge"
  ShowAttribute CharsetA              = "charset"
  ShowAttribute CheckedA              = "checked"
  ShowAttribute CiteA                 = "cite"
  ShowAttribute ClassA                = "class"
  ShowAttribute CodeA                 = "code"
  ShowAttribute CodebaseA             = "codebase"
  ShowAttribute ColorA                = "color"
  ShowAttribute ColsA                 = "cols"
  ShowAttribute ColspanA              = "colspan"
  ShowAttribute ContentA              = "content"
  ShowAttribute ContenteditableA      = "contenteditable"
  ShowAttribute ContextmenuA          = "contextmenu"
  ShowAttribute ControlsA             = "controls"
  ShowAttribute CoordsA               = "coords"
  ShowAttribute CrossoriginA          = "crossorigin"
  ShowAttribute DataA                 = "data"
  ShowAttribute DatetimeA             = "datetime"
  ShowAttribute DefaultA              = "default"
  ShowAttribute DeferA                = "defer"
  ShowAttribute DirA                  = "dir"
  ShowAttribute DirnameA              = "dirname"
  ShowAttribute DisabledA             = "disabled"
  ShowAttribute DownloadA             = "download"
  ShowAttribute DraggableA            = "draggable"
  ShowAttribute DropzoneA             = "dropzone"
  ShowAttribute EnctypeA              = "enctype"
  ShowAttribute ForA                  = "for"
  ShowAttribute FormA                 = "form"
  ShowAttribute FormactionA           = "formaction"
  ShowAttribute FormenctypeA          = "formenctype"
  ShowAttribute FormmethodA           = "formmethod"
  ShowAttribute FormnovalidateA       = "formnovalidate"
  ShowAttribute FormtargetA           = "formtarget"
  ShowAttribute HeadersA              = "headers"
  ShowAttribute HeightA               = "height"
  ShowAttribute HiddenA               = "hidden"
  ShowAttribute HighA                 = "high"
  ShowAttribute HrefA                 = "href"
  ShowAttribute HreflangA             = "hreflang"
  ShowAttribute HttpEquivA            = "httpEquiv"
  ShowAttribute IconA                 = "icon"
  ShowAttribute IdA                   = "id"
  ShowAttribute IntegrityA            = "integrity"
  ShowAttribute IsmapA                = "ismap"
  ShowAttribute ItempropA             = "itemprop"
  ShowAttribute KeytypeA              = "keytype"
  ShowAttribute KindA                 = "kind"
  ShowAttribute LabelA                = "label"
  ShowAttribute LangA                 = "lang"
  ShowAttribute LanguageA             = "language"
  ShowAttribute ListA                 = "list"
  ShowAttribute LongdescA             = "longdesc"
  ShowAttribute LoopA                 = "loop"
  ShowAttribute LowA                  = "low"
  ShowAttribute ManifestA             = "manifest"
  ShowAttribute MaxA                  = "max"
  ShowAttribute MaxlengthA            = "maxlength"
  ShowAttribute MediaA                = "media"
  ShowAttribute MethodA               = "method"
  ShowAttribute MinA                  = "min"
  ShowAttribute MinlengthA            = "minlength"
  ShowAttribute MultipleA             = "multiple"
  ShowAttribute MutedA                = "muted"
  ShowAttribute NameA                 = "name"
  ShowAttribute NonceA                = "nonce"
  ShowAttribute NovalidateA           = "novalidate"
  ShowAttribute OpenA                 = "open"
  ShowAttribute OptimumA              = "optimum"
  ShowAttribute PatternA              = "pattern"
  ShowAttribute PingA                 = "ping"
  ShowAttribute PlaceholderA          = "placeholder"
  ShowAttribute PosterA               = "poster"
  ShowAttribute PreloadA              = "preload"
  ShowAttribute RadiogroupA           = "radiogroup"
  ShowAttribute ReadonlyA             = "readonly"
  ShowAttribute ReferrerpolicyA       = "referrerpolicy"
  ShowAttribute RelA                  = "rel"
  ShowAttribute RequiredA             = "required"
  ShowAttribute RevA                  = "rev"
  ShowAttribute ReversedA             = "reversed"
  ShowAttribute RowsA                 = "rows"
  ShowAttribute RowspanA              = "rowspan"
  ShowAttribute SandboxA              = "sandbox"
  ShowAttribute ScopeA                = "scope"
  ShowAttribute ScopedA               = "scoped"
  ShowAttribute SeamlessA             = "seamless"
  ShowAttribute SelectedA             = "selected"
  ShowAttribute ShapeA                = "shape"
  ShowAttribute SizeA                 = "size"
  ShowAttribute SizesA                = "sizes"
  ShowAttribute SlotA                 = "slot"
  ShowAttribute SpanA                 = "span"
  ShowAttribute SpellcheckA           = "spellcheck"
  ShowAttribute SrcA                  = "src"
  ShowAttribute SrcdocA               = "srcdoc"
  ShowAttribute SrclangA              = "srclang"
  ShowAttribute SrcsetA               = "srcset"
  ShowAttribute StartA                = "start"
  ShowAttribute StepA                 = "step"
  ShowAttribute StyleA                = "style"
  ShowAttribute SummaryA              = "summary"
  ShowAttribute TabindexA             = "tabindex"
  ShowAttribute TargetA               = "target"
  ShowAttribute TitleA                = "title"
  ShowAttribute TranslateA            = "translate"
  ShowAttribute TypeA                 = "type"
  ShowAttribute TypemustmatchA        = "typemustmatch"
  ShowAttribute UsemapA               = "usemap"
  ShowAttribute ValueA                = "value"
  ShowAttribute WidthA                = "width"
  ShowAttribute WrapA                 = "wrap"
  ShowAttribute (CustomA x)           = x
