{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeInType             #-}
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

-- | 4 The elements of HTML
data Element (name :: Symbol) (categories :: [ContentCategory]) (contentModel :: ContentCategory) (contentAttributes :: [Symbol]) where

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
    (ManifestA & '[])

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
    (HrefA & TargetA & '[])

  -- | \ 4.2.4
  Link
    :: Element
    "link"
    '[Metadata, Flow, Phrasing]
    None
    (HrefA & CrossoriginA & RelA & MediaA & IntegrityA & HreflangA & TypeA & ReferrerpolicyA & SizesA & ImagesrcsetA & ImagesizesA & AsA & RelA & ColorA & DisabledA & '[])

  -- | \ 4.2.5
  Meta
    :: Element
    "meta"
    '[Metadata, Flow, Phrasing]
    None
    (NameA & HttpEquivA & ContentA & CharsetA & '[])

  -- | \ 4.2.6
  Style
    :: Element
    "style"
    '[Metadata]
    -- Text that gives a conformant style sheet.
    OnlyText
    (MediaA & '[])

  -- | \ 4.3 Sections
  --     4.3.1
  Body
    :: Element
    "body"
    '[]
    Flow
    (OnafterprintA & OnbeforeprintA & OnbeforeunloadA & OnhashchangeA & OnlanguagechangeA & OnmessageA & OnmessageerrorA & OnofflineA & OnonlineA & OnpagehideA & OnpageshowA & OnpopstateA & OnrejectionhandledA & OnstorageA & OnunhandledrejectionA & OnunloadA & '[])

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
    (CiteA & '[])

  -- | \ 4.4.5
  Ol
    :: Element
    "ol"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    (ReversedA & StartA & TypeA & '[])

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
    (ValueA & '[])

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
    (HrefA & TargetA & DownloadA & PingA & RelA & HreflangA & TypeA & ReferrerpolicyA & '[])

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
    (CiteA & '[])

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
    (ValueA & '[])

  -- | \ 4.5.14
  Time
    :: Element
    "time"
    '[Flow, Phrasing, Palpable]
    Phrasing
    (DatetimeA & '[])

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
    (CiteA & DatetimeA & '[])

  -- | \ 4.7.2
  Del
    :: Element
    "del"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    (CiteA & DatetimeA & '[])

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
    (SrcA & TypeA & SrcsetA & SizesA & MediaA & '[])

  -- | \ 4.8.3
  Img
    :: Element
    "img"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    (AltA & SrcA & SrcsetA & SizesA & CrossoriginA & UsemapA & IsmapA & WidthA & HeightA & ReferrerpolicyA & DecodingA & LoadingA & '[])

  -- | \ 4.8.5
  Iframe
    :: Element
    "iframe"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    (SrcA & SrcdocA & NameA & SandboxA & AllowA & AllowfullscreenA & WidthA & HeightA & ReferrerpolicyA & LoadingA & '[])

  -- | \ 4.8.6
  Embed
    :: Element
    "embed"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    (SrcA & TypeA & WidthA & HeightA & '[])

  -- | \ 4.8.7
  Object
    :: Element
    "object"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    (Elements '["param"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable)
    (DataA & TypeA & NameA & UsemapA & FormA & WidthA & HeightA & '[])

  -- | \ 4.8.8
  Param
    :: Element
    "param"
    '[]
    None
    (NameA & ValueA & '[])

  -- | \ 4.8.9
  Video
    :: Element
    "video"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements ["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements ["audio", "video"]))
    (SrcA & CrossoriginA & PosterA & PreloadA & AutoplayA & PlaysinlineA & LoopA & MutedA & ControlsA & WidthA & HeightA & '[])

  -- | \ 4.8.10
  Audio
    :: Element
    "audio"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements ["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements ["audio", "video"]))
    (SrcA & CrossoriginA & PreloadA & AutoplayA & LoopA & MutedA & ControlsA & '[])

  -- | \ 4.8.11
  Track
    :: Element
    "track"
    '[]
    None
    (KindA & SrcA & SrclangA & LabelA & DefaultA & '[])

  -- | \ 4.8.13
  Map
    :: Element
    "map"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    (NameA & '[])

  -- | \ 4.8.14
  Area
    :: Element
    "area"
    '[Flow, Phrasing]
    None
    (AltA & CoordsA & ShapeA & HrefA & TargetA & DownloadA & PingA & RelA & ReferrerpolicyA & '[])

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
    (SpanA & '[])

  -- | \ 4.9.4
  Col
    :: Element
    "col"
    '[]
    None
    (SpanA & '[])

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
    (ColspanA & RowspanA & HeadersA & '[])

  -- | \ 4.9.10
  Th
    :: Element
    "th"
    '[]
    (Flow :&: NOT (Elements ["header", "footer"] :|: Sectioning :|: Heading))
    (ColspanA & RowspanA & HeadersA & ScopeA & AbbrA & '[])

  -- | \ 4.10 Forms
  --     4.10.3
  Form
    :: Element
    "form"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["form"]))
    (AcceptCharsetA & ActionA & AutocompleteA & EnctypeA & MethodA & NameA & NovalidateA & TargetA & RelA & '[])

  -- | \ 4.10.4
  Label
    :: Element
    "label"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT (Elements '["label"]))
    (ForA & '[])

  -- | \ 4.10.5
  Input
    :: Element
    "input"
    '[Flow, Phrasing, Interactive, Palpable]
    None
    (AcceptA & AltA & AutocompleteA & CheckedA & DirnameA & DisabledA & FormA & FormactionA & FormenctypeA & FormmethodA & FormnovalidateA & FormtargetA & HeightA & ListA & MaxA & MaxlengthA & MinA & MinlengthA & MultipleA & NameA & PatternA & PlaceholderA & ReadonlyA & RequiredA & SizeA & SrcA & StepA & TypeA & ValueA & WidthA & '[])

  -- | \ 4.10.6
  Button
    :: Element
    "button"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT Interactive)
    (DisabledA & FormA & FormactionA & FormenctypeA & FormmethodA & FormnovalidateA & FormtargetA & NameA & TypeA & ValueA & '[])

  -- | \ 4.10.7
  Select
    :: Element
    "select"
    '[Flow, Phrasing, Interactive, Palpable]
    (Elements ["option", "optgroup"] :|: Scripting)
    (AutocompleteA & DisabledA & FormA & MultipleA & NameA & RequiredA & SizeA & '[])

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
    (DisabledA & LabelA & '[])

  -- | \ 4.10.10
  Option
    :: Element
    "option"
    '[]
    OnlyText
    (DisabledA & LabelA & SelectedA & ValueA & '[])

  -- | \ 4.10.11
  Textarea
    :: Element
    "textarea"
    '[Flow, Phrasing, Interactive, Palpable]
    OnlyText
    (AutocompleteA & ColsA & DirnameA & DisabledA & FormA & MaxlengthA & MinlengthA & NameA & PlaceholderA & ReadonlyA & RequiredA & RowsA & WrapA & '[])

  -- | \ 4.10.12
  Output
    :: Element
    "output"
    '[Flow, Phrasing, Palpable]
    Phrasing
    (ForA & FormA & NameA & '[])

  -- | \ 4.10.13
  Progress
    :: Element
    "progress"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["progress"]))
    (ValueA & MaxA & '[])

  -- | \ 4.10.14
  Meter
    :: Element
    "meter"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["meter"]))
    (ValueA & MinA & MaxA & LowA & HighA & OptimumA & '[])

  -- | \ 4.10.15
  Fieldset
    :: Element
    "fieldset"
    '[Flow, Palpable]
    (Elements '["legend"] :|: Flow)
    (DisabledA & FormA & NameA & '[])

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
    (OpenA & '[])

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
    (OpenA & '[])

  -- | \ 4.12 Scripting
  --     4.12.1
  Script
    :: Element
    "script"
    '[Metadata, Flow, Phrasing, Scripting]
    OnlyText
    (SrcA & TypeA & NomoduleA & AsyncA & DeferA & CrossoriginA & IntegrityA & ReferrerpolicyA & '[])

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
    (NameA & '[])

  -- | \ 4.12.5
  Canvas
    :: Element
    "canvas"
    '[Flow, Phrasing, Embedded, Palpable]
    (((Flow :|: Phrasing :|: Embedded :|: Palpable) :&: NOT Interactive) :|: Elements ["a", "img", "button", "input", "select"])
    (WidthA & HeightA & '[])

  -- | \ 4.13 Custom elements
  CustomElement
    :: Element
    name
    categories
    contentModel
    contentAttributes

data Attribute name global boolean where
  CustomA                     :: Attribute name global boolean

  -- List of attributes (excluding event handler content attributes)
  AbbrA                       :: Attribute "abbr"                        False False
  AcceptA                     :: Attribute "accept"                      False False
  AcceptCharsetA              :: Attribute "accept-charset"              False False
  AccesskeyA                  :: Attribute "accesskey"                   True  False
  ActionA                     :: Attribute "action"                      False False
  AllowA                      :: Attribute "allow"                       False False
  AllowfullscreenA            :: Attribute "allowfullscreen"             False True
  AltA                        :: Attribute "alt"                         False False
  AsA                         :: Attribute "as"                          False False
  AsyncA                      :: Attribute "async"                       False True
  AutocapitalizeA             :: Attribute "autocapitalize"              True  False
  AutocompleteA               :: Attribute "autocomplete"                False False
  AutofocusA                  :: Attribute "autofocus"                   True  True
  AutoplayA                   :: Attribute "autoplay"                    False True
  CharsetA                    :: Attribute "charset"                     False False
  CheckedA                    :: Attribute "checked"                     False True
  CiteA                       :: Attribute "cite"                        False False
  ClassA                      :: Attribute "class"                       True  False
  ColorA                      :: Attribute "color"                       False False
  ColsA                       :: Attribute "cols"                        False False
  ColspanA                    :: Attribute "colspan"                     False False
  ContentA                    :: Attribute "content"                     False False
  ContenteditableA            :: Attribute "contenteditable"             True  False
  ControlsA                   :: Attribute "controls"                    False True
  CoordsA                     :: Attribute "coords"                      False False
  CrossoriginA                :: Attribute "crossorigin"                 False False
  DataA                       :: Attribute "data"                        False False
  DatetimeA                   :: Attribute "datetime"                    False False
  DecodingA                   :: Attribute "decoding"                    False False
  DefaultA                    :: Attribute "default"                     False True
  DeferA                      :: Attribute "defer"                       False True
  DirA                        :: Attribute "dir"                         True  False
  DirnameA                    :: Attribute "dirname"                     False False
  DisabledA                   :: Attribute "disabled"                    False True
  DownloadA                   :: Attribute "download"                    False False
  DraggableA                  :: Attribute "draggable"                   True  False
  EnctypeA                    :: Attribute "enctype"                     False False
  EnterkeyhintA               :: Attribute "enterkeyhint"                True  False
  ForA                        :: Attribute "for"                         False False
  FormA                       :: Attribute "form"                        False False
  FormactionA                 :: Attribute "formaction"                  False False
  FormenctypeA                :: Attribute "formenctype"                 False False
  FormmethodA                 :: Attribute "formmethod"                  False False
  FormnovalidateA             :: Attribute "formnovalidate"              False True
  FormtargetA                 :: Attribute "formtarget"                  False False
  HeadersA                    :: Attribute "headers"                     False False
  HeightA                     :: Attribute "height"                      False False
  HiddenA                     :: Attribute "hidden"                      True  True
  HighA                       :: Attribute "high"                        False False
  HrefA                       :: Attribute "href"                        False False
  HreflangA                   :: Attribute "hreflang"                    False False
  HttpEquivA                  :: Attribute "httpEquiv"                   False False
  IdA                         :: Attribute "id"                          True  False
  ImagesizesA                 :: Attribute "imagesizes"                  False False
  ImagesrcsetA                :: Attribute "imagesrcset"                 False False
  InputmodeA                  :: Attribute "inputmode"                   True  False
  IntegrityA                  :: Attribute "integrity"                   False False
  IsA                         :: Attribute "is"                          True  False
  IsmapA                      :: Attribute "ismap"                       False True
  ItemidA                     :: Attribute "itemid"                      True  False
  ItempropA                   :: Attribute "itemprop"                    True  False
  ItemrefA                    :: Attribute "itemref"                     True  False
  ItemscopeA                  :: Attribute "itemscope"                   True  True
  ItemtypeA                   :: Attribute "itemtype"                    True  False
  KindA                       :: Attribute "kind"                        False False
  LabelA                      :: Attribute "label"                       False False
  LangA                       :: Attribute "lang"                        True  False
  ListA                       :: Attribute "list"                        False False
  LoadingA                    :: Attribute "loading"                     False False
  LoopA                       :: Attribute "loop"                        False True
  LowA                        :: Attribute "low"                         False False
  ManifestA                   :: Attribute "manifest"                    False False
  MaxA                        :: Attribute "max"                         False False
  MaxlengthA                  :: Attribute "maxlength"                   False False
  MediaA                      :: Attribute "media"                       False False
  MethodA                     :: Attribute "method"                      False False
  MinA                        :: Attribute "min"                         False False
  MinlengthA                  :: Attribute "minlength"                   False False
  MultipleA                   :: Attribute "multiple"                    False True
  MutedA                      :: Attribute "muted"                       False True
  NameA                       :: Attribute "name"                        False False
  NomoduleA                   :: Attribute "nomodule"                    False True
  NonceA                      :: Attribute "nonce"                       True  False
  NovalidateA                 :: Attribute "novalidate"                  False True
  OpenA                       :: Attribute "open"                        False True
  OptimumA                    :: Attribute "optimum"                     False False
  PatternA                    :: Attribute "pattern"                     False False
  PingA                       :: Attribute "ping"                        False False
  PlaceholderA                :: Attribute "placeholder"                 False False
  PlaysinlineA                :: Attribute "playsinline"                 False True
  PosterA                     :: Attribute "poster"                      False False
  PreloadA                    :: Attribute "preload"                     False False
  ReadonlyA                   :: Attribute "readonly"                    False True
  ReferrerpolicyA             :: Attribute "referrerpolicy"              False False
  RelA                        :: Attribute "rel"                         False False
  RequiredA                   :: Attribute "required"                    False True
  ReversedA                   :: Attribute "reversed"                    False True
  RowsA                       :: Attribute "rows"                        False False
  RowspanA                    :: Attribute "rowspan"                     False False
  SandboxA                    :: Attribute "sandbox"                     False False
  ScopeA                      :: Attribute "scope"                       False False
  SelectedA                   :: Attribute "selected"                    False True
  ShapeA                      :: Attribute "shape"                       False False
  SizeA                       :: Attribute "size"                        False False
  SizesA                      :: Attribute "sizes"                       False False
  SlotA                       :: Attribute "slot"                        True  False
  SpanA                       :: Attribute "span"                        False False
  SpellcheckA                 :: Attribute "spellcheck"                  True  False
  SrcA                        :: Attribute "src"                         False False
  SrcdocA                     :: Attribute "srcdoc"                      False False
  SrclangA                    :: Attribute "srclang"                     False False
  SrcsetA                     :: Attribute "srcset"                      False False
  StartA                      :: Attribute "start"                       False False
  StepA                       :: Attribute "step"                        False False
  StyleA                      :: Attribute "style"                       True  False
  TabindexA                   :: Attribute "tabindex"                    True  False
  TargetA                     :: Attribute "target"                      False False
  TitleA                      :: Attribute "title"                       True  False
  TranslateA                  :: Attribute "translate"                   True  False
  TypeA                       :: Attribute "type"                        False False
  UsemapA                     :: Attribute "usemap"                      False False
  ValueA                      :: Attribute "value"                       False False
  WidthA                      :: Attribute "width"                       False False
  WrapA                       :: Attribute "wrap"                        False False

  -- List of event handler content attributes
  OnabortA                    :: Attribute "onabort"                     True  False
  OnauxclickA                 :: Attribute "onauxclick"                  True  False
  OnafterprintA               :: Attribute "onafterprint"                False False
  OnbeforeprintA              :: Attribute "onbeforeprint"               False False
  OnbeforeunloadA             :: Attribute "onbeforeunload"              False False
  OnblurA                     :: Attribute "onblur"                      True  False
  OncancelA                   :: Attribute "oncancel"                    True  False
  OncanplayA                  :: Attribute "oncanplay"                   True  False
  OncanplaythroughA           :: Attribute "oncanplaythrough"            True  False
  OnchangeA                   :: Attribute "onchange"                    True  False
  OnclickA                    :: Attribute "onclick"                     True  False
  OncloseA                    :: Attribute "onclose"                     True  False
  OncontextmenuA              :: Attribute "oncontextmenu"               True  False
  OncopyA                     :: Attribute "oncopy"                      True  False
  OncuechangeA                :: Attribute "oncuechange"                 True  False
  OncutA                      :: Attribute "oncut"                       True  False
  OndblclickA                 :: Attribute "ondblclick"                  True  False
  OndragA                     :: Attribute "ondrag"                      True  False
  OndragendA                  :: Attribute "ondragend"                   True  False
  OndragenterA                :: Attribute "ondragenter"                 True  False
  OndragleaveA                :: Attribute "ondragleave"                 True  False
  OndragoverA                 :: Attribute "ondragover"                  True  False
  OndragstartA                :: Attribute "ondragstart"                 True  False
  OndropA                     :: Attribute "ondrop"                      True  False
  OndurationchangeA           :: Attribute "ondurationchange"            True  False
  OnemptiedA                  :: Attribute "onemptied"                   True  False
  OnendedA                    :: Attribute "onended"                     True  False
  OnerrorA                    :: Attribute "onerror"                     True  False
  OnfocusA                    :: Attribute "onfocus"                     True  False
  OnformdataA                 :: Attribute "onformdata"                  True  False
  OnhashchangeA               :: Attribute "onhashchange"                False False
  OninputA                    :: Attribute "oninput"                     True  False
  OninvalidA                  :: Attribute "oninvalid"                   True  False
  OnkeydownA                  :: Attribute "onkeydown"                   True  False
  OnkeypressA                 :: Attribute "onkeypress"                  True  False
  OnkeyupA                    :: Attribute "onkeyup"                     True  False
  OnlanguagechangeA           :: Attribute "onlanguagechange"            False False
  OnloadA                     :: Attribute "onload"                      True  False
  OnloadeddataA               :: Attribute "onloadeddata"                True  False
  OnloadedmetadataA           :: Attribute "onloadedmetadata"            True  False
  OnloadstartA                :: Attribute "onloadstart"                 True  False
  OnmessageA                  :: Attribute "onmessage"                   False False
  OnmessageerrorA             :: Attribute "onmessageerror"              False False
  OnmousedownA                :: Attribute "onmousedown"                 True  False
  OnmouseenterA               :: Attribute "onmouseenter"                True  False
  OnmouseleaveA               :: Attribute "onmouseleave"                True  False
  OnmousemoveA                :: Attribute "onmousemove"                 True  False
  OnmouseoutA                 :: Attribute "onmouseout"                  True  False
  OnmouseoverA                :: Attribute "onmouseover"                 True  False
  OnmouseupA                  :: Attribute "onmouseup"                   True  False
  OnofflineA                  :: Attribute "onoffline"                   False False
  OnonlineA                   :: Attribute "ononline"                    False False
  OnpagehideA                 :: Attribute "onpagehide"                  False False
  OnpageshowA                 :: Attribute "onpageshow"                  False False
  OnpasteA                    :: Attribute "onpaste"                     True  False
  OnpauseA                    :: Attribute "onpause"                     True  False
  OnplayA                     :: Attribute "onplay"                      True  False
  OnplayingA                  :: Attribute "onplaying"                   True  False
  OnpopstateA                 :: Attribute "onpopstate"                  False False
  OnprogressA                 :: Attribute "onprogress"                  True  False
  OnratechangeA               :: Attribute "onratechange"                True  False
  OnresetA                    :: Attribute "onreset"                     True  False
  OnresizeA                   :: Attribute "onresize"                    True  False
  OnrejectionhandledA         :: Attribute "onrejectionhandled"          False False
  OnscrollA                   :: Attribute "onscroll"                    True  False
  OnsecuritypolicyviolationA  :: Attribute "onsecuritypolicyviolation"   True  False
  OnseekedA                   :: Attribute "onseeked"                    True  False
  OnseekingA                  :: Attribute "onseeking"                   True  False
  OnselectA                   :: Attribute "onselect"                    True  False
  OnslotchangeA               :: Attribute "onslotchange"                True  False
  OnstalledA                  :: Attribute "onstalled"                   True  False
  OnstorageA                  :: Attribute "onstorage"                   False False
  OnsubmitA                   :: Attribute "onsubmit"                    True  False
  OnsuspendA                  :: Attribute "onsuspend"                   True  False
  OntimeupdateA               :: Attribute "ontimeupdate"                True  False
  OntoggleA                   :: Attribute "ontoggle"                    True  False
  OnunhandledrejectionA       :: Attribute "onunhandledrejection"        False False
  OnunloadA                   :: Attribute "onunload"                    False False
  OnvolumechangeA             :: Attribute "onvolumechange"              True  False
  OnwaitingA                  :: Attribute "onwaiting"                   True  False
  OnwheelA                    :: Attribute "onwheel"                     True  False

  -- [2020-11-03] ARIA https://w3c.github.io/aria/#states_and_properties
  RoleA                       :: Attribute "role"                        True  False

  -- 6.7 Definitios of States and Properties (all aria-* attributes)
  AriaActivedescendantA       :: Attribute "aria-activedescendant"       True  False
  AriaAtomicA                 :: Attribute "aria-atomic"                 True  False
  AriaAutocompleteA           :: Attribute "aria-autocomplete"           True  False
  AriaBraillelableA           :: Attribute "aria-braillelable"           True  False
  AriaBrailleroledescriptionA :: Attribute "aria-brailleroledescription" True  False
  AriaBusyA                   :: Attribute "aria-busy"                   True  False
  AriaCheckedA                :: Attribute "aria-checked"                True  False
  AriaColcountA               :: Attribute "aria-colcount"               True  False
  AriaColindexA               :: Attribute "aria-colindex"               True  False
  AriaColindextextA           :: Attribute "aria-colindextext"           True  False
  AriaColspanA                :: Attribute "aria-colspan"                True  False
  AriaControlsA               :: Attribute "aria-controls"               True  False
  AriaCurrentA                :: Attribute "aria-current"                True  False
  AriaDescribedbyA            :: Attribute "aria-describedby"            True  False
  AriaDescriptionA            :: Attribute "aria-description"            True  False
  AriaDetailsA                :: Attribute "aria-details"                True  False
  AriaDisabledA               :: Attribute "aria-disabled"               True  False
  AriaDropeffectA             :: Attribute "aria-dropeffect"             True  False
  AriaErrormessageA           :: Attribute "aria-errormessage"           True  False
  AriaExpandedA               :: Attribute "aria-expanded"               True  False
  AriaFlowtoA                 :: Attribute "aria-flowto"                 True  False
  AriaGrabbedA                :: Attribute "aria-grabbed"                True  False
  AriaHaspopupA               :: Attribute "aria-haspopup"               True  False
  AriaHiddenA                 :: Attribute "aria-hidden"                 True  False
  AriaInvalidA                :: Attribute "aria-invalid"                True  False
  AriaKeyshortcutsA           :: Attribute "aria-keyshortcuts"           True  False
  AriaLabelA                  :: Attribute "aria-label"                  True  False
  AriaLabelledByA             :: Attribute "aria-labelledBy"             True  False
  AriaLevelA                  :: Attribute "aria-level"                  True  False
  AriaLiveA                   :: Attribute "aria-live"                   True  False
  AriaModalA                  :: Attribute "aria-modal"                  True  False
  AriaMultilineA              :: Attribute "aria-multiline"              True  False
  AriaMultiselectableA        :: Attribute "aria-multiselectable"        True  False
  AriaOrientationA            :: Attribute "aria-orientation"            True  False
  AriaOwnsA                   :: Attribute "aria-owns"                   True  False
  AriaPlaceholderA            :: Attribute "aria-placeholder"            True  False
  AriaPosinsetA               :: Attribute "aria-posinset"               True  False
  AriaPressedA                :: Attribute "aria-pressed"                True  False
  AriaReadonlyA               :: Attribute "aria-readonly"               True  False
  AriaRelevantA               :: Attribute "aria-relevant"               True  False
  AriaRequiredA               :: Attribute "aria-required"               True  False
  AriaRoledescriptionA        :: Attribute "aria-roledescription"        True  False
  AriaRowcountA               :: Attribute "aria-rowcount"               True  False
  AriaRowindexA               :: Attribute "aria-rowindex"               True  False
  AriaRowindextextA           :: Attribute "aria-rowindextext"           True  False
  AriaRowspanA                :: Attribute "aria-rowspan"                True  False
  AriaSelectedA               :: Attribute "aria-selected"               True  False
  AriaSetsizeA                :: Attribute "aria-setsize"                True  False
  AriaSortA                   :: Attribute "aria-sort"                   True  False
  AriaValuemaxA               :: Attribute "aria-valuemax"               True  False
  AriaValueminA               :: Attribute "aria-valuemin"               True  False
  AriaValuenowA               :: Attribute "aria-valuenow"               True  False
  AriaValuetextA              :: Attribute "aria-valuetext"              True  False

-- |
-- = Utilities

type GetAttributeName (e :: Attribute a global boolean) = a

type (&) k b = GetAttributeName k ': b

newtype Lawless a = Lawless a

infixr 5 &

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
  ToList (Attribute a global boolean) = 'List '[] (AppendSymbol " " a)
  ToList (Attribute a global boolean := ())       = 'List '[] (AppendSymbol " " a)
  ToList (Attribute a global boolean := b)        = AppendSymbol " " (AppendSymbol a "=\"") <| ToList b |> "\""
  ToList ()              = 'List '[] ""
  ToList (Proxy x)       = 'List '[] x
  ToList x               = 'List '[""] ""

-- | Combine two elements or attributes sequentially.
--
-- >>> I # Div
-- <i></i><div></div>
--
-- >>> I :@ (IdA:="a" # ClassA:="b") :> "c"
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
  Lawful relation x (c := _) = Lawful relation x c

  Lawful AttributeValue (Attribute name1 global1 boolean1) (Attribute name2 global2 boolean2) = TypeError (Text "The attribute " :<>: Text name1 :<>: Text " can't contain the attribute " :<>: Text name2 :<>: Text ".")
  Lawful AttributeValue (Attribute name1 global1 boolean1) (Element name2 categories contentModel contentAttributes) = TypeError (Text "The attribute " :<>: Text name1 :<>: Text " can't contain the element " :<>: Text name2 :<>: Text ".")
  Lawful AttributeValue (Attribute name global boolean) v = If boolean (TypeError (Text "The attribute " :<>: Text name :<>: Text " is boolean and can't contain a value.")) (() :: Constraint)
  Lawful AttributeValue x v = TypeError (ShowType x :<>: Text " is not an attribute.")

  Lawful Fatherhood (e :@ _) c = Lawful Fatherhood e c
  Lawful Fatherhood (Element name categories contentModel contentAttributes) (Attribute name2 global boolean) = TypeError (Text name :<>: Text " can't have an attribute as children.")
  Lawful Fatherhood (Element name categories None contentAttributes) _ = TypeError (Text name :<>: Text " can't have children.")

  Lawful Fatherhood (Element name1 categories1 contentModel1 contentAttributes1)
               (Element name2 categories2 contentModel2 contentAttributes2) = MaybeTypeError name1 (Text name2) (CheckContentCategory name2 contentModel1 categories2)
  Lawful Fatherhood (Element name categories contentModel contentAttributes) string = MaybeTypeError name (ShowType string) (CheckContentCategory "" contentModel '[OnlyText, Flow, Phrasing])
  Lawful Fatherhood _ _ = TypeError (Text "Only Elements and Elements with Attributes can father children.")

  Lawful Attribution (Element name categories contentModel contentAttributes) (Attribute a global boolean)
    = If (global || Elem a contentAttributes)
    (() :: Constraint)
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

data (:=) a v where
  (:=) :: Lawful AttributeValue a v => a -> v -> a := v

infixr 6 :>
infixr 9 :@
infixr 9 :=

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw {fromRaw :: a}

type family Null xs where
  Null '[] = True
  Null _ = False

type family Length c where
  Length (a :> b) = Length a + Length b
  Length (a :@ b) = Length b
  Length (a # b)       = Length a + Length b
  Length (_ := b)      = Length b
  Length (Lawless a)   = Length a
  Length (Attribute a global boolean) = 0
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
  GetV (a := b)      = GetV b
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
