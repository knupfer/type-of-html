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


-- | 4 The elements of HTML
data family Element (name :: Symbol) (categories :: [ContentCategory]) (contentModel :: ContentCategory) (contentAttributes :: [Symbol])

data instance Element
    "!DOCTYPE html"
    '[]
    None
    '[]
  = DOCTYPE

  -- | \ 4.1 The document element
  --     4.1.1
data instance Element
    "html"
    '[]
    -- A head element followed by a body element.
    (Elements '["head", "body"])
    '["manifest"]
  = Html

  -- | \ 4.2 Document metadata
  --     4.2.1
data instance Element
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
  = Head

  -- | \ 4.2.2
data instance Element
    "title"
    '[Metadata]
    -- Text that is not inter-element whitespace.
    OnlyText
    '[]
  = Title

  -- | \ 4.2.3
data instance Element
    "base"
    '[Metadata]
    None
    '["href", "target"]
  = Base

  -- | \ 4.2.4
data instance Element
    "link"
    '[Metadata, Flow, Phrasing]
    None
    '["href", "crossorigin", "rel", "media", "integrity", "hreflang", "type", "referrerpolicy", "sizes", "imagesrcset", "imagesizes", "as", "rel", "color", "disabled"]
  = Link

  -- | \ 4.2.5
data instance Element
    "meta"
    '[Metadata, Flow, Phrasing]
    None
    '["name", "httpequiv", "content", "charset"]
  = Meta

  -- | \ 4.2.6
data instance Element
    "style"
    '[Metadata]
    -- Text that gives a conformant style sheet.
    OnlyText
    '["media"]
  = Style

  -- | \ 4.3 Sections
  --     4.3.1
data instance Element
    "body"
    '[]
    Flow
    '["onafterprint", "onbeforeprint", "onbeforeunload", "onhashchange", "onlanguagechange", "onmessage", "onmessageerror", "onoffline", "ononline", "onpagehide", "onpageshow", "onpopstate", "onrejectionhandled", "onstorage", "onunhandledrejection", "onunload"]
  = Body

  -- | \ 4.3.2
data instance Element
    "article"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Article

  -- | \ 4.3.3
data instance Element
    "section"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Section

  -- | \ 4.3.4
data instance Element
    "nav"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Nav

  -- | \ 4.3.5
data instance Element
    "aside"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Aside

  -- | \ 4.3.6
data instance Element
    "h1"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H1

data instance Element
    "h2"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H2

data instance Element
    "h3"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H3

data instance Element
    "h4"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H4

data instance Element
    "h5"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H5

data instance Element
    "h6"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H6

  -- | \ 4.3.7
data instance Element
    "hgroup"
    '[Flow, Heading, Palpable]
    ((Heading :&: NOT (Elements '["hgroup"])) :|: Scripting)
    '[]
  = Hgroup

  -- | \ 4.3.8
data instance Element
    "header"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]
  = Header

  -- | \ 4.3.9
data instance Element
    "footer"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]
  = Footer

  -- | \ 4.3.10
data instance Element
    "address"
    '[Flow, Palpable]
    -- Flow content, but with no heading content descendants, no
    -- sectioning content descendants, and no header, footer, or
    -- address element descendants.
    (Flow :&: NOT (Heading :|: Sectioning :|: Elements '["header", "footer", "address"]))
    '[]
  = Address

  -- | \ 4.4 Grouping content
  --     4.4.1
data instance Element
    "p"
    '[Flow, Palpable]
    Phrasing
    '[]
  = P

  -- | \ 4.4.2
data instance Element
    "hr"
    '[Flow]
    None
    '[]
  = Hr

  -- | \ 4.4.3
data instance Element
    "pre"
    '[Flow, Palpable]
    Phrasing
    '[]
  = Pre

  -- | \ 4.4.4
data instance Element
    "blockquote"
    '[Flow, Palpable]
    Flow
    '["cite"]
  = Blockquote

  -- | \ 4.4.5
data instance Element
    "ol"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '["reversed", "start", "type"]
  = Ol

  -- | \ 4.4.6
data instance Element
    "ul"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]
  = Ul

  -- | \ 4.4.7
data instance Element
    "menu"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]
  = Menu

  -- | \ 4.4.8
data instance Element
    "li"
    '[]
    Flow
    '["value"]
  = Li

  -- | \ 4.4.9
data instance Element
    "dl"
    '[Flow, Palpable]
    (Elements '["dt", "dd", "div"] :|: Scripting)
    '[]
  = Dl

  -- | \ 4.4.10
data instance Element
    "dt"
    '[]
    (Flow :&: NOT (Sectioning :|: Heading :|: Elements '["header", "footer"]))
    '[]
  = Dt

  -- | \ 4.4.11
data instance Element
    "dd"
    '[]
    Flow
    '[]
  = Dd

  -- | \ 4.4.12
data instance Element
    "figure"
    '[Flow, Palpable]
    -- Either: one figcaption element followed by flow content. Or:
    -- flow content followed by one figcaption element. Or: flow
    -- content.
    (Flow :|: Elements '["figcaption"])
    '[]
  = Figure

  -- | \ 4.4.13
data instance Element
    "figcaption"
    '[]
    Flow
    '[]
  = Figcaption

  -- | \ 4.4.14
data instance Element
    "main"
    '[Flow, Palpable]
    Flow
    '[]
  = Main

  -- | \ 4.4.15
data instance Element
    "div"
    '[Flow, Palpable]
    -- If the element is a child of a dl element: one or more dt
    -- elements followed by one or more dd elements, optionally
    -- intermixed with script-supporting elements. If the element is
    -- not a child of a dl element: flow content.
    (Flow :|: Elements '["dt", "dt"] :|: Scripting)
    '[]
  = Div

  -- | \ 4.5 Text-level semantics
  --     4.5.1
data instance Element
    "a"
    '[Flow, Phrasing, Interactive, Palpable]
    -- Transparent, but there must be no interactive content
    -- descendant, a element descendant, or descendant with the
    -- tabindex attribute specified.
    ((Flow :|: Phrasing :|: Palpable) :&: NOT (Elements '["a"]))
    '["href", "target", "download", "ping", "rel", "hreflang", "type", "referrerpolicy"]
  = A

  -- | \ 4.5.2
data instance Element
    "em"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Em

  -- | \ 4.5.3
data instance Element
    "strong"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Strong

  -- | \ 4.5.4
data instance Element
    "small"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Small

  -- | \ 4.5.5
data instance Element
    "s"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = S

  -- | \ 4.5.6
data instance Element
    "cite"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Cite

  -- | \ 4.5.7
data instance Element
    "q"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["cite"]
  = Q

  -- | \ 4.5.8
data instance Element
    "dfn"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["dfn"]))
    '[]
  = Dfn

  -- | \ 4.5.9
data instance Element
    "abbr"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Abbr

  -- | \ 4.5.10
data instance Element
    "ruby"
    '[Flow, Phrasing, Palpable]
    (Phrasing :|: Elements '["rt", "rp"])
    '[]
  = Ruby

  -- | \ 4.5.11
data instance Element
    "rt"
    '[]
    Phrasing
    '[]
  = Rt

  -- | \ 4.5.12
data instance Element
    "rp"
    '[]
    OnlyText
    '[]
  = Rp

  -- | \ 4.5.13
data instance Element
    "data"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["value"]
  = Data

  -- | \ 4.5.14
data instance Element
    "time"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["datetime"]
  = Time

  -- | \ 4.5.15
data instance Element
    "code"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Code

  -- | \ 4.5.16
data instance Element
    "var"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Var

  -- | \ 4.5.17
data instance Element
    "samp"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Samp

  -- | \ 4.5.18
data instance Element
    "kbd"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Kbd

  -- | \ 4.5.19
data instance Element
    "sub"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Sub

data instance Element
    "sup"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Sup

  -- | \ 4.5.20
data instance Element
    "i"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = I

  -- | \ 4.5.21
data instance Element
    "b"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = B

  -- | \ 4.5.22
data instance Element
    "u"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = U

  -- | \ 4.5.23
data instance Element
    "mark"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Mark

  -- | \ 4.5.24
data instance Element
    "bdi"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Bdi

  -- | \ 4.5.25
data instance Element
    "bdo"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Bdo

  -- | \ 4.5.26
data instance Element
    "span"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Span

  -- | \ 4.5.27
data instance Element
    "br"
    '[Flow, Phrasing]
    None
    '[]
  = Br

  -- | \ 4.5.28
data instance Element
    "wbr"
    '[Flow, Phrasing]
    None
    '[]
  = Wbr

  -- | \ 4.7 Edits
  --     4.7.1
data instance Element
    "ins"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["cite", "datetime"]
  = Ins

  -- | \ 4.7.2
data instance Element
    "del"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["cite", "datetime"]
  = Del

  -- | \ 4.8 Embedded content
  --     4.8.1
data instance Element
    "picture"
    '[Flow, Phrasing, Embedded]
    (Elements '["source", "img"] :|: Scripting)
    '[]
  = Picture

  -- | \ 4.8.2
data instance Element
    "source"
    '[]
    None
    '["src", "type", "srcset", "sizes", "media"]
  = Source

  -- | \ 4.8.3
data instance Element
    "img"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["alt", "src", "srcset", "sizes", "crossorigin", "usemap", "ismap", "width", "height", "referrerpolicy", "decoding", "loading"]
  = Img

  -- | \ 4.8.5
data instance Element
    "iframe"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "srcdoc", "name", "sandbox", "allow", "allowfullscreen", "width", "height", "referrerpolicy", "loading"]
  = Iframe

  -- | \ 4.8.6
data instance Element
    "embed"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "type", "width", "height"]
  = Embed

  -- | \ 4.8.7
data instance Element
    "object"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    (Elements '["param"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable)
    '["data", "type", "name", "usemap", "form", "width", "height"]
  = Object

  -- | \ 4.8.8
data instance Element
    "param"
    '[]
    None
    '["name", "value"]
  = Param

  -- | \ 4.8.9
data instance Element
    "video"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "poster", "preload", "autoplay", "playsinline", "loop", "muted", "controls", "width", "height"]
  = Video

  -- | \ 4.8.10
data instance Element
    "audio"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "preload", "autoplay", "loop", "muted", "controls"]
  = Audio

  -- | \ 4.8.11
data instance Element
    "track"
    '[]
    None
    '["kind", "src", "srclang", "label", "default"]
  = Track

  -- | \ 4.8.13
data instance Element
    "map"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["name"]
  = Map

  -- | \ 4.8.14
data instance Element
    "area"
    '[Flow, Phrasing]
    None
    '["alt", "coords", "shape", "href", "target", "download", "ping", "rel", "referrerpolicy"]
  = Area

  -- | \ 4.9 Tabular data
  --     4.9.1
data instance Element
    "table"
    '[Flow, Palpable]
    (Elements '["caption", "colgroup", "thead", "tbody", "tr", "tfoot"] :|: Scripting)
    '[]
  = Table

  -- | \ 4.9.2
data instance Element
    "caption"
    '[]
    (Flow :|: NOT (Elements '["table"]))
    '[]
  = Caption

  -- | \ 4.9.3
data instance Element
    "colgroup"
    '[]
    (Elements '["col", "template"])
    '["span"]
  = Colgroup

  -- | \ 4.9.4
data instance Element
    "col"
    '[]
    None
    '["span"]
  = Col

  -- | \ 4.9.5
data instance Element
    "tbody"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Tbody

  -- | \ 4.9.6
data instance Element
    "thead"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Thead

  -- | \ 4.9.7
data instance Element
    "tfoot"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Tfoot

  -- | \ 4.9.8
data instance Element
    "tr"
    '[]
    (Elements '["td", "th"] :|: Scripting)
    '[]
  = Tr

  -- | \ 4.9.9
data instance Element
    "td"
    '[]
    Flow
    '["colspan", "rowspan", "headers"]
  = Td

  -- | \ 4.9.10
data instance Element
    "th"
    '[]
    (Flow :&: NOT (Elements '["header", "footer"] :|: Sectioning :|: Heading))
    '["colspan", "rowspan", "headers", "scope", "abbr"]
  = Th

  -- | \ 4.10 Forms
  --     4.10.3
data instance Element
    "form"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["form"]))
    '["acceptcharset", "action", "autocomplete", "enctype", "method", "name", "novalidate", "target", "rel"]
  = Form

  -- | \ 4.10.4
data instance Element
    "label"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT (Elements '["label"]))
    '["for"]
  = Label

  -- | \ 4.10.5
data instance Element
    "input"
    '[Flow, Phrasing, Interactive, Palpable]
    None
    '["accept", "alt", "autocomplete", "checked", "dirname", "disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "height", "list", "max", "maxlength", "min", "minlength", "multiple", "name", "pattern", "placeholder", "readonly", "required", "size", "src", "step", "type", "value", "width"]
  = Input

  -- | \ 4.10.6
data instance Element
    "button"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT Interactive)
    '["disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "name", "type", "value"]
  = Button

  -- | \ 4.10.7
data instance Element
    "select"
    '[Flow, Phrasing, Interactive, Palpable]
    (Elements '["option", "optgroup"] :|: Scripting)
    '["autocomplete", "disabled", "form", "multiple", "name", "required", "size"]
  = Select

  -- | \ 4.10.8
data instance Element
    "datalist"
    '[Flow, Phrasing]
    (Phrasing :|: Scripting :|: Elements '["option"])
    '[]
  = Datalist

  -- | \ 4.10.9
data instance Element
    "optgroup"
    '[]
    (Elements '["option"] :|: Scripting)
    '["disabled", "label"]
  = Optgroup

  -- | \ 4.10.10
data instance Element
    "option"
    '[]
    OnlyText
    '["disabled", "label", "selected", "value"]
  = Option

  -- | \ 4.10.11
data instance Element
    "textarea"
    '[Flow, Phrasing, Interactive, Palpable]
    OnlyText
    '["autocomplete", "cols", "dirname", "disabled", "form", "maxlength", "minlength", "name", "placeholder", "readonly", "required", "rows", "wrap"]
  = Textarea

  -- | \ 4.10.12
data instance Element
    "output"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["for", "form", "name"]
  = Output

  -- | \ 4.10.13
data instance Element
    "progress"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["progress"]))
    '["value", "max"]
  = Progress

  -- | \ 4.10.14
data instance Element
    "meter"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["meter"]))
    '["value", "min", "max", "low", "high", "optimum"]
  = Meter

  -- | \ 4.10.15
data instance Element
    "fieldset"
    '[Flow, Palpable]
    (Elements '["legend"] :|: Flow)
    '["disabled", "form", "name"]
  = Fieldset

  -- | \ 4.10.16
data instance Element
    "legend"
    '[]
    (Phrasing :|: Heading)
    '[]
  = Legend

  -- | \ 4.11 Interactive elements
  --     4.11.1
data instance Element
    "details"
    '[Flow, Interactive, Palpable]
    (Elements '["summary"] :|: Flow)
    '["open"]
  = Details

  -- | \ 4.11.2
data instance Element
    "summary"
    '[]
    (Phrasing :|: Heading)
    '[]
  = Summary

  -- | \ 4.11.4
data instance Element
    "dialog"
    '[Flow]
    Flow
    '["open"]
  = Dialog

  -- | \ 4.12 Scripting
  --     4.12.1
data instance Element
    "script"
    '[Metadata, Flow, Phrasing, Scripting]
    OnlyText
    '["src", "type", "nomodule", "async", "defer", "crossorigin", "integrity", "referrerpolicy"]
  = Script

  -- | \ 4.12.2
data instance Element
    "noscript"
    '[Metadata, Flow, Phrasing]
    ((Elements '["link", "style", "meta"] :|: Metadata :|: Flow :|: Phrasing) :&: NOT (Elements '["noscript"]))
    '[]
  = Noscript

  -- | \ 4.12.3
data instance Element
    "template"
    '[Metadata, Flow, Phrasing, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Palpable)
    '[]
  = Template

  -- | \ 4.12.4
data instance Element
    "slot"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["name"]
  = Slot

  -- | \ 4.12.5
data instance Element
    "canvas"
    '[Flow, Phrasing, Embedded, Palpable]
    (((Flow :|: Phrasing :|: Embedded :|: Palpable) :&: NOT Interactive) :|: Elements '["a", "img", "button", "input", "select"])
    '["width", "height"]
  = Canvas

data family Attribute (name :: Symbol) (global :: Bool) (boolean :: Bool)

  -- List of attributes (excluding event handler content attributes)
data instance Attribute "abbr"                      False False = AbbrA
data instance Attribute "accept"                    False False = AcceptA
data instance Attribute "accept-charset"            False False = AcceptCharsetA
data instance Attribute "accesskey"                 True  False = AccesskeyA
data instance Attribute "action"                    False False = ActionA
data instance Attribute "allow"                     False False = AllowA
data instance Attribute "allowfullscreen"           False True  = AllowfullscreenA
data instance Attribute "alt"                       False False = AltA
data instance Attribute "as"                        False False = AsA
data instance Attribute "async"                     False True  = AsyncA
data instance Attribute "autocapitalize"            True  False = AutocapitalizeA
data instance Attribute "autocomplete"              False False = AutocompleteA
data instance Attribute "autofocus"                 True  True  = AutofocusA
data instance Attribute "autoplay"                  False True  = AutoplayA
data instance Attribute "charset"                   False False = CharsetA
data instance Attribute "checked"                   False True  = CheckedA
data instance Attribute "cite"                      False False = CiteA
data instance Attribute "class"                     True  False = ClassA
data instance Attribute "color"                     False False = ColorA
data instance Attribute "cols"                      False False = ColsA
data instance Attribute "colspan"                   False False = ColspanA
data instance Attribute "content"                   False False = ContentA
data instance Attribute "contenteditable"           True  False = ContenteditableA
data instance Attribute "controls"                  False True  = ControlsA
data instance Attribute "coords"                    False False = CoordsA
data instance Attribute "crossorigin"               False False = CrossoriginA
data instance Attribute "data"                      False False = DataA
data instance Attribute "datetime"                  False False = DatetimeA
data instance Attribute "decoding"                  False False = DecodingA
data instance Attribute "default"                   False True  = DefaultA
data instance Attribute "defer"                     False True  = DeferA
data instance Attribute "dir"                       True  False = DirA
data instance Attribute "dirname"                   False False = DirnameA
data instance Attribute "disabled"                  False True  = DisabledA
data instance Attribute "download"                  False False = DownloadA
data instance Attribute "draggable"                 True  False = DraggableA
data instance Attribute "enctype"                   False False = EnctypeA
data instance Attribute "enterkeyhint"              True  False = EnterkeyhintA
data instance Attribute "for"                       False False = ForA
data instance Attribute "form"                      False False = FormA
data instance Attribute "formaction"                False False = FormactionA
data instance Attribute "formenctype"               False False = FormenctypeA
data instance Attribute "formmethod"                False False = FormmethodA
data instance Attribute "formnovalidate"            False True  = FormnovalidateA
data instance Attribute "formtarget"                False False = FormtargetA
data instance Attribute "headers"                   False False = HeadersA
data instance Attribute "height"                    False False = HeightA
data instance Attribute "hidden"                    True  True  = HiddenA
data instance Attribute "high"                      False False = HighA
data instance Attribute "href"                      False False = HrefA
data instance Attribute "hreflang"                  False False = HreflangA
data instance Attribute "httpEquiv"                 False False = HttpEquivA
data instance Attribute "id"                        True  False = IdA
data instance Attribute "imagesizes"                False False = ImagesizesA
data instance Attribute "imagesrcset"               False False = ImagesrcsetA
data instance Attribute "inputmode"                 True  False = InputmodeA
data instance Attribute "integrity"                 False False = IntegrityA
data instance Attribute "is"                        True  False = IsA
data instance Attribute "ismap"                     False True  = IsmapA
data instance Attribute "itemid"                    True  False = ItemidA
data instance Attribute "itemprop"                  True  False = ItempropA
data instance Attribute "itemref"                   True  False = ItemrefA
data instance Attribute "itemscope"                 True  True  = ItemscopeA
data instance Attribute "itemtype"                  True  False = ItemtypeA
data instance Attribute "kind"                      False False = KindA
data instance Attribute "label"                     False False = LabelA
data instance Attribute "lang"                      True  False = LangA
data instance Attribute "list"                      False False = ListA
data instance Attribute "loading"                   False False = LoadingA
data instance Attribute "loop"                      False True  = LoopA
data instance Attribute "low"                       False False = LowA
data instance Attribute "manifest"                  False False = ManifestA
data instance Attribute "max"                       False False = MaxA
data instance Attribute "maxlength"                 False False = MaxlengthA
data instance Attribute "media"                     False False = MediaA
data instance Attribute "method"                    False False = MethodA
data instance Attribute "min"                       False False = MinA
data instance Attribute "minlength"                 False False = MinlengthA
data instance Attribute "multiple"                  False True  = MultipleA
data instance Attribute "muted"                     False True  = MutedA
data instance Attribute "name"                      False False = NameA
data instance Attribute "nomodule"                  False True  = NomoduleA
data instance Attribute "nonce"                     True  False = NonceA
data instance Attribute "novalidate"                False True  = NovalidateA
data instance Attribute "open"                      False True  = OpenA
data instance Attribute "optimum"                   False False = OptimumA
data instance Attribute "pattern"                   False False = PatternA
data instance Attribute "ping"                      False False = PingA
data instance Attribute "placeholder"               False False = PlaceholderA
data instance Attribute "playsinline"               False True  = PlaysinlineA
data instance Attribute "poster"                    False False = PosterA
data instance Attribute "preload"                   False False = PreloadA
data instance Attribute "readonly"                  False True  = ReadonlyA
data instance Attribute "referrerpolicy"            False False = ReferrerpolicyA
data instance Attribute "rel"                       False False = RelA
data instance Attribute "required"                  False True  = RequiredA
data instance Attribute "reversed"                  False True  = ReversedA
data instance Attribute "rows"                      False False = RowsA
data instance Attribute "rowspan"                   False False = RowspanA
data instance Attribute "sandbox"                   False False = SandboxA
data instance Attribute "scope"                     False False = ScopeA
data instance Attribute "selected"                  False True  = SelectedA
data instance Attribute "shape"                     False False = ShapeA
data instance Attribute "size"                      False False = SizeA
data instance Attribute "sizes"                     False False = SizesA
data instance Attribute "slot"                      True  False = SlotA
data instance Attribute "span"                      False False = SpanA
data instance Attribute "spellcheck"                True  False = SpellcheckA
data instance Attribute "src"                       False False = SrcA
data instance Attribute "srcdoc"                    False False = SrcdocA
data instance Attribute "srclang"                   False False = SrclangA
data instance Attribute "srcset"                    False False = SrcsetA
data instance Attribute "start"                     False False = StartA
data instance Attribute "step"                      False False = StepA
data instance Attribute "style"                     True  False = StyleA
data instance Attribute "tabindex"                  True  False = TabindexA
data instance Attribute "target"                    False False = TargetA
data instance Attribute "title"                     True  False = TitleA
data instance Attribute "translate"                 True  False = TranslateA
data instance Attribute "type"                      False False = TypeA
data instance Attribute "usemap"                    False False = UsemapA
data instance Attribute "value"                     False False = ValueA
data instance Attribute "width"                     False False = WidthA
data instance Attribute "wrap"                      False False = WrapA

  -- List of event handler content attributes
data instance Attribute "onabort"                   True  False = OnabortA
data instance Attribute "onauxclick"                True  False = OnauxclickA
data instance Attribute "onafterprint"              False False = OnafterprintA
data instance Attribute "onbeforeprint"             False False = OnbeforeprintA
data instance Attribute "onbeforeunload"            False False = OnbeforeunloadA
data instance Attribute "onblur"                    True  False = OnblurA
data instance Attribute "oncancel"                  True  False = OncancelA
data instance Attribute "oncanplay"                 True  False = OncanplayA
data instance Attribute "oncanplaythrough"          True  False = OncanplaythroughA
data instance Attribute "onchange"                  True  False = OnchangeA
data instance Attribute "onclick"                   True  False = OnclickA
data instance Attribute "onclose"                   True  False = OncloseA
data instance Attribute "oncontextmenu"             True  False = OncontextmenuA
data instance Attribute "oncopy"                    True  False = OncopyA
data instance Attribute "oncuechange"               True  False = OncuechangeA
data instance Attribute "oncut"                     True  False = OncutA
data instance Attribute "ondblclick"                True  False = OndblclickA
data instance Attribute "ondrag"                    True  False = OndragA
data instance Attribute "ondragend"                 True  False = OndragendA
data instance Attribute "ondragenter"               True  False = OndragenterA
data instance Attribute "ondragleave"               True  False = OndragleaveA
data instance Attribute "ondragover"                True  False = OndragoverA
data instance Attribute "ondragstart"               True  False = OndragstartA
data instance Attribute "ondrop"                    True  False = OndropA
data instance Attribute "ondurationchange"          True  False = OndurationchangeA
data instance Attribute "onemptied"                 True  False = OnemptiedA
data instance Attribute "onended"                   True  False = OnendedA
data instance Attribute "onerror"                   True  False = OnerrorA
data instance Attribute "onfocus"                   True  False = OnfocusA
data instance Attribute "onformdata"                True  False = OnformdataA
data instance Attribute "onhashchange"              False False = OnhashchangeA
data instance Attribute "oninput"                   True  False = OninputA
data instance Attribute "oninvalid"                 True  False = OninvalidA
data instance Attribute "onkeydown"                 True  False = OnkeydownA
data instance Attribute "onkeypress"                True  False = OnkeypressA
data instance Attribute "onkeyup"                   True  False = OnkeyupA
data instance Attribute "onlanguagechange"          False False = OnlanguagechangeA
data instance Attribute "onload"                    True  False = OnloadA
data instance Attribute "onloadeddata"              True  False = OnloadeddataA
data instance Attribute "onloadedmetadata"          True  False = OnloadedmetadataA
data instance Attribute "onloadstart"               True  False = OnloadstartA
data instance Attribute "onmessage"                 False False = OnmessageA
data instance Attribute "onmessageerror"            False False = OnmessageerrorA
data instance Attribute "onmousedown"               True  False = OnmousedownA
data instance Attribute "onmouseenter"              True  False = OnmouseenterA
data instance Attribute "onmouseleave"              True  False = OnmouseleaveA
data instance Attribute "onmousemove"               True  False = OnmousemoveA
data instance Attribute "onmouseout"                True  False = OnmouseoutA
data instance Attribute "onmouseover"               True  False = OnmouseoverA
data instance Attribute "onmouseup"                 True  False = OnmouseupA
data instance Attribute "onoffline"                 False False = OnofflineA
data instance Attribute "ononline"                  False False = OnonlineA
data instance Attribute "onpagehide"                False False = OnpagehideA
data instance Attribute "onpageshow"                False False = OnpageshowA
data instance Attribute "onpaste"                   True  False = OnpasteA
data instance Attribute "onpause"                   True  False = OnpauseA
data instance Attribute "onplay"                    True  False = OnplayA
data instance Attribute "onplaying"                 True  False = OnplayingA
data instance Attribute "onpopstate"                False False = OnpopstateA
data instance Attribute "onprogress"                True  False = OnprogressA
data instance Attribute "onratechange"              True  False = OnratechangeA
data instance Attribute "onreset"                   True  False = OnresetA
data instance Attribute "onresize"                  True  False = OnresizeA
data instance Attribute "onrejectionhandled"        False False = OnrejectionhandledA
data instance Attribute "onscroll"                  True  False = OnscrollA
data instance Attribute "onsecuritypolicyviolation" True  False = OnsecuritypolicyviolationA
data instance Attribute "onseeked"                  True  False = OnseekedA
data instance Attribute "onseeking"                 True  False = OnseekingA
data instance Attribute "onselect"                  True  False = OnselectA
data instance Attribute "onslotchange"              True  False = OnslotchangeA
data instance Attribute "onstalled"                 True  False = OnstalledA
data instance Attribute "onstorage"                 False False = OnstorageA
data instance Attribute "onsubmit"                  True  False = OnsubmitA
data instance Attribute "onsuspend"                 True  False = OnsuspendA
data instance Attribute "ontimeupdate"              True  False = OntimeupdateA
data instance Attribute "ontoggle"                  True  False = OntoggleA
data instance Attribute "onunhandledrejection"      False False = OnunhandledrejectionA
data instance Attribute "onunload"                  False False = OnunloadA
data instance Attribute "onvolumechange"            True  False = OnvolumechangeA
data instance Attribute "onwaiting"                 True  False = OnwaitingA
data instance Attribute "onwheel"                   True  False = OnwheelA

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
