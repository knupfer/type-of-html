{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}

module Html
  ( module Html.Type
  , module Html.Convert
  , module Html.Render
  , module Html
  ) where

import Html.Convert
import Html.Type
import Html.Render

-- |
-- = Elements

-- | 4 The elements of HTML
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

-- |
-- = Attributes

-- List of attributes (excluding event handler content attributes)
newtype instance Attribute "abbr"                      False v  = AbbrA v
newtype instance Attribute "accept"                    False v  = AcceptA v
newtype instance Attribute "accept-charset"            False v  = AcceptCharsetA v
newtype instance Attribute "accesskey"                 True  v  = AccesskeyA v
newtype instance Attribute "action"                    False v  = ActionA v
newtype instance Attribute "allow"                     False v  = AllowA v
data    instance Attribute "allowfullscreen"           False () = AllowfullscreenA
newtype instance Attribute "alt"                       False v  = AltA v
newtype instance Attribute "as"                        False v  = AsA v
data    instance Attribute "async"                     False () = AsyncA
newtype instance Attribute "autocapitalize"            True  v  = AutocapitalizeA v
newtype instance Attribute "autocomplete"              False v  = AutocompleteA v
data    instance Attribute "autofocus"                 True  () = AutofocusA
data    instance Attribute "autoplay"                  False () = AutoplayA
newtype instance Attribute "charset"                   False v  = CharsetA v
data    instance Attribute "checked"                   False () = CheckedA
newtype instance Attribute "cite"                      False v  = CiteA v
newtype instance Attribute "class"                     True  v  = ClassA v
newtype instance Attribute "color"                     False v  = ColorA v
newtype instance Attribute "cols"                      False v  = ColsA v
newtype instance Attribute "colspan"                   False v  = ColspanA v
newtype instance Attribute "content"                   False v  = ContentA v
newtype instance Attribute "contenteditable"           True  v  = ContenteditableA v
data    instance Attribute "controls"                  False () = ControlsA
newtype instance Attribute "coords"                    False v  = CoordsA v
newtype instance Attribute "crossorigin"               False v  = CrossoriginA v
newtype instance Attribute "data"                      False v  = DataA v
newtype instance Attribute "datetime"                  False v  = DatetimeA v
newtype instance Attribute "decoding"                  False v  = DecodingA v
data    instance Attribute "default"                   False () = DefaultA
data    instance Attribute "defer"                     False () = DeferA
newtype instance Attribute "dir"                       True  v  = DirA v
newtype instance Attribute "dirname"                   False v  = DirnameA v
data    instance Attribute "disabled"                  False () = DisabledA
newtype instance Attribute "download"                  False v  = DownloadA v
newtype instance Attribute "draggable"                 True  v  = DraggableA v
newtype instance Attribute "enctype"                   False v  = EnctypeA v
newtype instance Attribute "enterkeyhint"              True  v  = EnterkeyhintA v
newtype instance Attribute "for"                       False v  = ForA v
newtype instance Attribute "form"                      False v  = FormA v
newtype instance Attribute "formaction"                False v  = FormactionA v
newtype instance Attribute "formenctype"               False v  = FormenctypeA v
newtype instance Attribute "formmethod"                False v  = FormmethodA v
data    instance Attribute "formnovalidate"            False () = FormnovalidateA
newtype instance Attribute "formtarget"                False v  = FormtargetA v
newtype instance Attribute "headers"                   False v  = HeadersA v
newtype instance Attribute "height"                    False v  = HeightA v
data    instance Attribute "hidden"                    True  () = HiddenA
newtype instance Attribute "high"                      False v  = HighA v
newtype instance Attribute "href"                      False v  = HrefA v
newtype instance Attribute "hreflang"                  False v  = HreflangA v
newtype instance Attribute "httpEquiv"                 False v  = HttpEquivA v
newtype instance Attribute "id"                        True  v  = IdA v
newtype instance Attribute "imagesizes"                False v  = ImagesizesA v
newtype instance Attribute "imagesrcset"               False v  = ImagesrcsetA v
newtype instance Attribute "inputmode"                 True  v  = InputmodeA v
newtype instance Attribute "integrity"                 False v  = IntegrityA v
newtype instance Attribute "is"                        True  v  = IsA v
data    instance Attribute "ismap"                     False () = IsmapA
newtype instance Attribute "itemid"                    True  v  = ItemidA v
newtype instance Attribute "itemprop"                  True  v  = ItempropA v
newtype instance Attribute "itemref"                   True  v  = ItemrefA v
data    instance Attribute "itemscope"                 True  () = ItemscopeA
newtype instance Attribute "itemtype"                  True  v  = ItemtypeA v
newtype instance Attribute "kind"                      False v  = KindA v
newtype instance Attribute "label"                     False v  = LabelA v
newtype instance Attribute "lang"                      True  v  = LangA v
newtype instance Attribute "list"                      False v  = ListA v
newtype instance Attribute "loading"                   False v  = LoadingA v
data    instance Attribute "loop"                      False () = LoopA
newtype instance Attribute "low"                       False v  = LowA v
newtype instance Attribute "manifest"                  False v  = ManifestA v
newtype instance Attribute "max"                       False v  = MaxA v
newtype instance Attribute "maxlength"                 False v  = MaxlengthA v
newtype instance Attribute "media"                     False v  = MediaA v
newtype instance Attribute "method"                    False v  = MethodA v
newtype instance Attribute "min"                       False v  = MinA v
newtype instance Attribute "minlength"                 False v  = MinlengthA v
data    instance Attribute "multiple"                  False () = MultipleA
data    instance Attribute "muted"                     False () = MutedA
newtype instance Attribute "name"                      False v  = NameA v
data    instance Attribute "nomodule"                  False () = NomoduleA
newtype instance Attribute "nonce"                     True  v  = NonceA v
data    instance Attribute "novalidate"                False () = NovalidateA
data    instance Attribute "open"                      False () = OpenA
newtype instance Attribute "optimum"                   False v  = OptimumA v
newtype instance Attribute "pattern"                   False v  = PatternA v
newtype instance Attribute "ping"                      False v  = PingA v
newtype instance Attribute "placeholder"               False v  = PlaceholderA v
data    instance Attribute "playsinline"               False () = PlaysinlineA
newtype instance Attribute "poster"                    False v  = PosterA v
newtype instance Attribute "preload"                   False v  = PreloadA v
data    instance Attribute "readonly"                  False () = ReadonlyA
newtype instance Attribute "referrerpolicy"            False v  = ReferrerpolicyA v
newtype instance Attribute "rel"                       False v  = RelA v
data    instance Attribute "required"                  False () = RequiredA
data    instance Attribute "reversed"                  False () = ReversedA
newtype instance Attribute "rows"                      False v  = RowsA v
newtype instance Attribute "rowspan"                   False v  = RowspanA v
newtype instance Attribute "sandbox"                   False v  = SandboxA v
newtype instance Attribute "scope"                     False v  = ScopeA v
data    instance Attribute "selected"                  False () = SelectedA
newtype instance Attribute "shape"                     False v  = ShapeA v
newtype instance Attribute "size"                      False v  = SizeA v
newtype instance Attribute "sizes"                     False v  = SizesA v
newtype instance Attribute "slot"                      True  v  = SlotA v
newtype instance Attribute "span"                      False v  = SpanA v
newtype instance Attribute "spellcheck"                True  v  = SpellcheckA v
newtype instance Attribute "src"                       False v  = SrcA v
newtype instance Attribute "srcdoc"                    False v  = SrcdocA v
newtype instance Attribute "srclang"                   False v  = SrclangA v
newtype instance Attribute "srcset"                    False v  = SrcsetA v
newtype instance Attribute "start"                     False v  = StartA v
newtype instance Attribute "step"                      False v  = StepA v
newtype instance Attribute "style"                     True  v  = StyleA v
newtype instance Attribute "tabindex"                  True  v  = TabindexA v
newtype instance Attribute "target"                    False v  = TargetA v
newtype instance Attribute "title"                     True  v  = TitleA v
newtype instance Attribute "translate"                 True  v  = TranslateA v
newtype instance Attribute "type"                      False v  = TypeA v
newtype instance Attribute "usemap"                    False v  = UsemapA v
newtype instance Attribute "value"                     False v  = ValueA v
newtype instance Attribute "width"                     False v  = WidthA v
newtype instance Attribute "wrap"                      False v  = WrapA v

-- List of event handler content attributes
newtype instance Attribute "onabort"                   True  v  = OnabortA v
newtype instance Attribute "onauxclick"                True  v  = OnauxclickA v
newtype instance Attribute "onafterprint"              False v  = OnafterprintA v
newtype instance Attribute "onbeforeprint"             False v  = OnbeforeprintA v
newtype instance Attribute "onbeforeunload"            False v  = OnbeforeunloadA v
newtype instance Attribute "onblur"                    True  v  = OnblurA v
newtype instance Attribute "oncancel"                  True  v  = OncancelA v
newtype instance Attribute "oncanplay"                 True  v  = OncanplayA v
newtype instance Attribute "oncanplaythrough"          True  v  = OncanplaythroughA v
newtype instance Attribute "onchange"                  True  v  = OnchangeA v
newtype instance Attribute "onclick"                   True  v  = OnclickA v
newtype instance Attribute "onclose"                   True  v  = OncloseA v
newtype instance Attribute "oncontextmenu"             True  v  = OncontextmenuA v
newtype instance Attribute "oncopy"                    True  v  = OncopyA v
newtype instance Attribute "oncuechange"               True  v  = OncuechangeA v
newtype instance Attribute "oncut"                     True  v  = OncutA v
newtype instance Attribute "ondblclick"                True  v  = OndblclickA v
newtype instance Attribute "ondrag"                    True  v  = OndragA v
newtype instance Attribute "ondragend"                 True  v  = OndragendA v
newtype instance Attribute "ondragenter"               True  v  = OndragenterA v
newtype instance Attribute "ondragleave"               True  v  = OndragleaveA v
newtype instance Attribute "ondragover"                True  v  = OndragoverA v
newtype instance Attribute "ondragstart"               True  v  = OndragstartA v
newtype instance Attribute "ondrop"                    True  v  = OndropA v
newtype instance Attribute "ondurationchange"          True  v  = OndurationchangeA v
newtype instance Attribute "onemptied"                 True  v  = OnemptiedA v
newtype instance Attribute "onended"                   True  v  = OnendedA v
newtype instance Attribute "onerror"                   True  v  = OnerrorA v
newtype instance Attribute "onfocus"                   True  v  = OnfocusA v
newtype instance Attribute "onformdata"                True  v  = OnformdataA v
newtype instance Attribute "onhashchange"              False v  = OnhashchangeA v
newtype instance Attribute "oninput"                   True  v  = OninputA v
newtype instance Attribute "oninvalid"                 True  v  = OninvalidA v
newtype instance Attribute "onkeydown"                 True  v  = OnkeydownA v
newtype instance Attribute "onkeypress"                True  v  = OnkeypressA v
newtype instance Attribute "onkeyup"                   True  v  = OnkeyupA v
newtype instance Attribute "onlanguagechange"          False v  = OnlanguagechangeA v
newtype instance Attribute "onload"                    True  v  = OnloadA v
newtype instance Attribute "onloadeddata"              True  v  = OnloadeddataA v
newtype instance Attribute "onloadedmetadata"          True  v  = OnloadedmetadataA v
newtype instance Attribute "onloadstart"               True  v  = OnloadstartA v
newtype instance Attribute "onmessage"                 False v  = OnmessageA v
newtype instance Attribute "onmessageerror"            False v  = OnmessageerrorA v
newtype instance Attribute "onmousedown"               True  v  = OnmousedownA v
newtype instance Attribute "onmouseenter"              True  v  = OnmouseenterA v
newtype instance Attribute "onmouseleave"              True  v  = OnmouseleaveA v
newtype instance Attribute "onmousemove"               True  v  = OnmousemoveA v
newtype instance Attribute "onmouseout"                True  v  = OnmouseoutA v
newtype instance Attribute "onmouseover"               True  v  = OnmouseoverA v
newtype instance Attribute "onmouseup"                 True  v  = OnmouseupA v
newtype instance Attribute "onoffline"                 False v  = OnofflineA v
newtype instance Attribute "ononline"                  False v  = OnonlineA v
newtype instance Attribute "onpagehide"                False v  = OnpagehideA v
newtype instance Attribute "onpageshow"                False v  = OnpageshowA v
newtype instance Attribute "onpaste"                   True  v  = OnpasteA v
newtype instance Attribute "onpause"                   True  v  = OnpauseA v
newtype instance Attribute "onplay"                    True  v  = OnplayA v
newtype instance Attribute "onplaying"                 True  v  = OnplayingA v
newtype instance Attribute "onpopstate"                False v  = OnpopstateA v
newtype instance Attribute "onprogress"                True  v  = OnprogressA v
newtype instance Attribute "onratechange"              True  v  = OnratechangeA v
newtype instance Attribute "onreset"                   True  v  = OnresetA v
newtype instance Attribute "onresize"                  True  v  = OnresizeA v
newtype instance Attribute "onrejectionhandled"        False v  = OnrejectionhandledA v
newtype instance Attribute "onscroll"                  True  v  = OnscrollA v
newtype instance Attribute "onsecuritypolicyviolation" True  v  = OnsecuritypolicyviolationA v
newtype instance Attribute "onseeked"                  True  v  = OnseekedA v
newtype instance Attribute "onseeking"                 True  v  = OnseekingA v
newtype instance Attribute "onselect"                  True  v  = OnselectA v
newtype instance Attribute "onslotchange"              True  v  = OnslotchangeA v
newtype instance Attribute "onstalled"                 True  v  = OnstalledA v
newtype instance Attribute "onstorage"                 False v  = OnstorageA v
newtype instance Attribute "onsubmit"                  True  v  = OnsubmitA v
newtype instance Attribute "onsuspend"                 True  v  = OnsuspendA v
newtype instance Attribute "ontimeupdate"              True  v  = OntimeupdateA v
newtype instance Attribute "ontoggle"                  True  v  = OntoggleA v
newtype instance Attribute "onunhandledrejection"      False v  = OnunhandledrejectionA v
newtype instance Attribute "onunload"                  False v  = OnunloadA v
newtype instance Attribute "onvolumechange"            True  v  = OnvolumechangeA v
newtype instance Attribute "onwaiting"                 True  v  = OnwaitingA v
newtype instance Attribute "onwheel"                   True  v  = OnwheelA v








