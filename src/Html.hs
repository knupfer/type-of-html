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

-- | \ 1.7.0.0 
type DOCTYPE = Element
    "!DOCTYPE html"
    '[]
    None
    '[]

-- | \ 4.1 The document element
--     4.1.1
data instance Element
    "html"
    '[]
    -- A head element followed by a body element.
    (Elements '["head", "body"])
    '["manifest"]
  = Html

-- | \ 1.7.0.0 
type Html = Element
    "html"
    '[]
    (Elements '["head", "body"])
    '["manifest"]

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

-- | \ 1.7.0.0 
type Head = Element
    "head"
    '[]
    Metadata
    '[]

-- | \ 4.2.2
data instance Element
    "title"
    '[Metadata]
    -- Text that is not inter-element whitespace.
    OnlyText
    '[]
  = Title

-- | \ 1.7.0.0 
type Title = Element
    "title"
    '[Metadata]
    OnlyText
    '[]

-- | \ 4.2.3
data instance Element
    "base"
    '[Metadata]
    None
    '["href", "target"]
  = Base

-- | \ 1.7.0.0 
type Base = Element
    "base"
    '[Metadata]
    None
    '["href", "target"]

-- | \ 4.2.4
data instance Element
    "link"
    '[Metadata, Flow, Phrasing]
    None
    '["href", "crossorigin", "rel", "media", "integrity", "hreflang", "type", "referrerpolicy", "sizes", "imagesrcset", "imagesizes", "as", "rel", "color", "disabled"]
  = Link

-- | \ 1.7.0.0 
type Link = Element
    "link"
    '[Metadata, Flow, Phrasing]
    None
    '["href", "crossorigin", "rel", "media", "integrity", "hreflang", "type", "referrerpolicy", "sizes", "imagesrcset", "imagesizes", "as", "rel", "color", "disabled"]

-- | \ 4.2.5
data instance Element
    "meta"
    '[Metadata, Flow, Phrasing]
    None
    '["name", "httpequiv", "content", "charset"]
  = Meta

-- | \ 1.7.0.0 
type Meta = Element
    "meta"
    '[Metadata, Flow, Phrasing]
    None
    '["name", "httpequiv", "content", "charset"]

-- | \ 4.2.6
data instance Element
    "style"
    '[Metadata]
    -- Text that gives a conformant style sheet.
    OnlyText
    '["media"]
  = Style

-- | \ 1.7.0.0 
type Style = Element
    "style"
    '[Metadata]
    OnlyText
    '["media"]

-- | \ 4.3 Sections
--     4.3.1
data instance Element
    "body"
    '[]
    Flow
    '["onafterprint", "onbeforeprint", "onbeforeunload", "onhashchange", "onlanguagechange", "onmessage", "onmessageerror", "onoffline", "ononline", "onpagehide", "onpageshow", "onpopstate", "onrejectionhandled", "onstorage", "onunhandledrejection", "onunload"]
  = Body

-- | \ 1.7.0.0 
type Body = Element
    "body"
    '[]
    Flow
    '["onafterprint", "onbeforeprint", "onbeforeunload", "onhashchange", "onlanguagechange", "onmessage", "onmessageerror", "onoffline", "ononline", "onpagehide", "onpageshow", "onpopstate", "onrejectionhandled", "onstorage", "onunhandledrejection", "onunload"]

-- | \ 4.3.2
data instance Element
    "article"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Article

-- | \ 1.7.0.0 
type Article = Element
    "article"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

-- | \ 4.3.3
data instance Element
    "section"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Section

-- | \ 1.7.0.0 
type Section = Element
    "section"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

-- | \ 4.3.4
data instance Element
    "nav"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Nav

-- | \ 1.7.0.0 
type Nav = Element
    "nav"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

-- | \ 4.3.5
data instance Element
    "aside"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]
  = Aside

-- | \ 1.7.0.0 
type Aside = Element
    "aside"
    '[Flow, Sectioning, Palpable]
    Flow
    '[]

-- | \ 4.3.6
data instance Element
    "h1"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H1

-- | \ 1.7.0.0 
type H1 = Element
    "h1"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

data instance Element
    "h2"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H2

-- | \ 1.7.0.0 
type H2 = Element
    "h2"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

data instance Element
    "h3"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H3

-- | \ 1.7.0.0 
type H3 = Element
    "h3"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

data instance Element
    "h4"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H4

-- | \ 1.7.0.0 
type H4 = Element
    "h4"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

data instance Element
    "h5"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H5

-- | \ 1.7.0.0 
type H5 = Element
    "h5"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

data instance Element
    "h6"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]
  = H6

-- | \ 1.7.0.0 
type H6 = Element
    "h6"
    '[Flow, Heading, Palpable]
    Phrasing
    '[]

-- | \ 4.3.7
data instance Element
    "hgroup"
    '[Flow, Heading, Palpable]
    ((Heading :&: NOT (Elements '["hgroup"])) :|: Scripting)
    '[]
  = Hgroup

-- | \ 1.7.0.0 
type Hgroup = Element
    "hgroup"
    '[Flow, Heading, Palpable]
    ((Heading :&: NOT (Elements '["hgroup"])) :|: Scripting)
    '[]

-- | \ 4.3.8
data instance Element
    "header"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]
  = Header

-- | \ 1.7.0.0 
type Header = Element
    "header"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]

-- | \ 4.3.9
data instance Element
    "footer"
    '[Flow, Palpable]
    -- Flow content, but with no header or footer element descendants.
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]
  = Footer

-- | \ 1.7.0.0 
type Footer = Element
    "footer"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["header", "footer"]))
    '[]

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

-- | \ 1.7.0.0 
type Address = Element
    "address"
    '[Flow, Palpable]
    (Flow :&: NOT (Heading :|: Sectioning :|: Elements '["header", "footer", "address"]))
    '[]

-- | \ 4.4 Grouping content
--     4.4.1
data instance Element
    "p"
    '[Flow, Palpable]
    Phrasing
    '[]
  = P

-- | \ 1.7.0.0 
type P = Element
    "p"
    '[Flow, Palpable]
    Phrasing
    '[]

-- | \ 4.4.2
data instance Element
    "hr"
    '[Flow]
    None
    '[]
  = Hr

-- | \ 1.7.0.0 
type Hr = Element
    "hr"
    '[Flow]
    None
    '[]

-- | \ 4.4.3
data instance Element
    "pre"
    '[Flow, Palpable]
    Phrasing
    '[]
  = Pre

-- | \ 1.7.0.0 
type Pre = Element
    "pre"
    '[Flow, Palpable]
    Phrasing
    '[]

-- | \ 4.4.4
data instance Element
    "blockquote"
    '[Flow, Palpable]
    Flow
    '["cite"]
  = Blockquote

-- | \ 1.7.0.0 
type Blockquote = Element
    "blockquote"
    '[Flow, Palpable]
    Flow
    '["cite"]

-- | \ 4.4.5
data instance Element
    "ol"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '["reversed", "start", "type"]
  = Ol

-- | \ 1.7.0.0 
type Ol = Element
    "ol"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '["reversed", "start", "type"]

-- | \ 4.4.6
data instance Element
    "ul"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]
  = Ul

-- | \ 1.7.0.0 
type Ul = Element
    "ul"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]

-- | \ 4.4.7
data instance Element
    "menu"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]
  = Menu

-- | \ 1.7.0.0 
type Menu = Element
    "menu"
    '[Flow, Palpable]
    (Elements '["li"] :|: Scripting)
    '[]

-- | \ 4.4.8
data instance Element
    "li"
    '[]
    Flow
    '["value"]
  = Li

-- | \ 1.7.0.0 
type Li = Element
    "li"
    '[]
    Flow
    '["value"]

-- | \ 4.4.9
data instance Element
    "dl"
    '[Flow, Palpable]
    (Elements '["dt", "dd", "div"] :|: Scripting)
    '[]
  = Dl

-- | \ 1.7.0.0 
type Dl = Element
    "dl"
    '[Flow, Palpable]
    (Elements '["dt", "dd", "div"] :|: Scripting)
    '[]

-- | \ 4.4.10
data instance Element
    "dt"
    '[]
    (Flow :&: NOT (Sectioning :|: Heading :|: Elements '["header", "footer"]))
    '[]
  = Dt

-- | \ 1.7.0.0 
type Dt = Element
    "dt"
    '[]
    (Flow :&: NOT (Sectioning :|: Heading :|: Elements '["header", "footer"]))
    '[]

-- | \ 4.4.11
data instance Element
    "dd"
    '[]
    Flow
    '[]
  = Dd

-- | \ 1.7.0.0 
type Dd = Element
    "dd"
    '[]
    Flow
    '[]

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

-- | \ 1.7.0.0 
type Figure = Element
    "figure"
    '[Flow, Palpable]
    (Flow :|: Elements '["figcaption"])
    '[]

-- | \ 4.4.13
data instance Element
    "figcaption"
    '[]
    Flow
    '[]
  = Figcaption

-- | \ 1.7.0.0 
type Figcaption = Element
    "figcaption"
    '[]
    Flow
    '[]

-- | \ 4.4.14
data instance Element
    "main"
    '[Flow, Palpable]
    Flow
    '[]
  = Main

-- | \ 1.7.0.0 
type Main = Element
    "main"
    '[Flow, Palpable]
    Flow
    '[]

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

-- | \ 1.7.0.0 
type Div = Element
    "div"
    '[Flow, Palpable]
    (Flow :|: Elements '["dt", "dt"] :|: Scripting)
    '[]


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

-- | \ 1.7.0.0 
type A = Element
    "a"
    '[Flow, Phrasing, Interactive, Palpable]
    ((Flow :|: Phrasing :|: Palpable) :&: NOT (Elements '["a"]))
    '["href", "target", "download", "ping", "rel", "hreflang", "type", "referrerpolicy"]

-- | \ 4.5.2
data instance Element
    "em"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Em

-- | \ 1.7.0.0 
type Em = Element
    "em"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.3
data instance Element
    "strong"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Strong

-- | \ 1.7.0.0 
type Strong = Element
    "strong"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.4
data instance Element
    "small"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Small

-- | \ 1.7.0.0 
type Small = Element
    "small"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.5
data instance Element
    "s"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = S

-- | \ 1.7.0.0 
type S = Element
    "s"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.6
data instance Element
    "cite"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Cite

-- | \ 1.7.0.0 
type Cite = Element
    "cite"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.7
data instance Element
    "q"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["cite"]
  = Q

-- | \ 1.7.0.0 
type Q = Element
    "q"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["cite"]

-- | \ 4.5.8
data instance Element
    "dfn"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["dfn"]))
    '[]
  = Dfn

-- | \ 1.7.0.0 
type Dfn = Element
    "dfn"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["dfn"]))
    '[]

-- | \ 4.5.9
data instance Element
    "abbr"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Abbr

-- | \ 1.7.0.0 
type Abbr = Element
    "abbr"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.10
data instance Element
    "ruby"
    '[Flow, Phrasing, Palpable]
    (Phrasing :|: Elements '["rt", "rp"])
    '[]
  = Ruby

-- | \ 1.7.0.0 
type Ruby = Element
    "ruby"
    '[Flow, Phrasing, Palpable]
    (Phrasing :|: Elements '["rt", "rp"])
    '[]

-- | \ 4.5.11
data instance Element
    "rt"
    '[]
    Phrasing
    '[]
  = Rt

-- | \ 1.7.0.0 
type Rt = Element
    "rt"
    '[]
    Phrasing
    '[]

-- | \ 4.5.12
data instance Element
    "rp"
    '[]
    OnlyText
    '[]
  = Rp

-- | \ 1.7.0.0 
type Rp = Element
    "rp"
    '[]
    OnlyText
    '[]

-- | \ 4.5.13
data instance Element
    "data"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["value"]
  = Data

-- | \ 1.7.0.0 
type Data = Element
    "data"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["value"]

-- | \ 4.5.14
data instance Element
    "time"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["datetime"]
  = Time

-- | \ 1.7.0.0 
type Time = Element
    "time"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["datetime"]

-- | \ 4.5.15
data instance Element
    "code"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Code

-- | \ 1.7.0.0 
type Code = Element
    "code"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.16
data instance Element
    "var"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Var

-- | \ 1.7.0.0 
type Var = Element
    "var"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.17
data instance Element
    "samp"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Samp

-- | \ 1.7.0.0 
type Samp = Element
    "samp"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.18
data instance Element
    "kbd"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Kbd

-- | \ 1.7.0.0 
type Kbd = Element
    "kbd"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.19
data instance Element
    "sub"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Sub

-- | \ 1.7.0.0 
type Sub = Element
    "sub"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

data instance Element
    "sup"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Sup

-- | \ 1.7.0.0 
type Sup = Element
    "sup"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.20
data instance Element
    "i"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = I

-- | \ 1.7.0.0 
type I = Element
    "i"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.21
data instance Element
    "b"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = B

-- | \ 1.7.0.0 
type B = Element
    "b"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.22
data instance Element
    "u"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = U

-- | \ 1.7.0.0 
type U = Element
    "u"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.23
data instance Element
    "mark"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Mark

-- | \ 1.7.0.0 
type Mark = Element
    "mark"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.24
data instance Element
    "bdi"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Bdi

-- | \ 1.7.0.0 
type Bdi = Element
    "bdi"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.25
data instance Element
    "bdo"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Bdo

-- | \ 1.7.0.0 
type Bdo = Element
    "bdo"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.26
data instance Element
    "span"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]
  = Span

-- | \ 1.7.0.0 
type Span = Element
    "span"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '[]

-- | \ 4.5.27
data instance Element
    "br"
    '[Flow, Phrasing]
    None
    '[]
  = Br

-- | \ 1.7.0.0 
type Br = Element
    "br"
    '[Flow, Phrasing]
    None
    '[]

-- | \ 4.5.28
data instance Element
    "wbr"
    '[Flow, Phrasing]
    None
    '[]
  = Wbr

-- | \ 1.7.0.0 
type Wbr = Element
    "wbr"
    '[Flow, Phrasing]
    None
    '[]

-- | \ 4.7 Edits
--     4.7.1
data instance Element
    "ins"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["cite", "datetime"]
  = Ins

-- | \ 1.7.0.0 
type Ins = Element
    "ins"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["cite", "datetime"]

-- | \ 4.7.2
data instance Element
    "del"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["cite", "datetime"]
  = Del

-- | \ 1.7.0.0 
type Del = Element
    "del"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["cite", "datetime"]

-- | \ 4.8 Embedded content
--     4.8.1
data instance Element
    "picture"
    '[Flow, Phrasing, Embedded]
    (Elements '["source", "img"] :|: Scripting)
    '[]
  = Picture

-- | \ 1.7.0.0 
type Picture = Element
    "picture"
    '[Flow, Phrasing, Embedded]
    (Elements '["source", "img"] :|: Scripting)
    '[]

-- | \ 4.8.2
data instance Element
    "source"
    '[]
    None
    '["src", "type", "srcset", "sizes", "media"]
  = Source

-- | \ 1.7.0.0 
type Source = Element
    "source"
    '[]
    None
    '["src", "type", "srcset", "sizes", "media"]

-- | \ 4.8.3
data instance Element
    "img"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["alt", "src", "srcset", "sizes", "crossorigin", "usemap", "ismap", "width", "height", "referrerpolicy", "decoding", "loading"]
  = Img

-- | \ 1.7.0.0 
type Img = Element
    "img"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["alt", "src", "srcset", "sizes", "crossorigin", "usemap", "ismap", "width", "height", "referrerpolicy", "decoding", "loading"]

-- | \ 4.8.5
data instance Element
    "iframe"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "srcdoc", "name", "sandbox", "allow", "allowfullscreen", "width", "height", "referrerpolicy", "loading"]
  = Iframe

-- | \ 1.7.0.0 
type Iframe = Element
    "iframe"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "srcdoc", "name", "sandbox", "allow", "allowfullscreen", "width", "height", "referrerpolicy", "loading"]

-- | \ 4.8.6
data instance Element
    "embed"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "type", "width", "height"]
  = Embed

-- | \ 1.7.0.0 
type Embed = Element
    "embed"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    None
    '["src", "type", "width", "height"]

-- | \ 4.8.7
data instance Element
    "object"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    (Elements '["param"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable)
    '["data", "type", "name", "usemap", "form", "width", "height"]
  = Object

-- | \ 1.7.0.0 
type Object = Element
    "object"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    (Elements '["param"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable)
    '["data", "type", "name", "usemap", "form", "width", "height"]

-- | \ 4.8.8
data instance Element
    "param"
    '[]
    None
    '["name", "value"]
  = Param

-- | \ 1.7.0.0 
type Param = Element
    "param"
    '[]
    None
    '["name", "value"]

-- | \ 4.8.9
data instance Element
    "video"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "poster", "preload", "autoplay", "playsinline", "loop", "muted", "controls", "width", "height"]
  = Video

-- | \ 1.7.0.0 
type Video = Element
    "video"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "poster", "preload", "autoplay", "playsinline", "loop", "muted", "controls", "width", "height"]

-- | \ 4.8.10
data instance Element
    "audio"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "preload", "autoplay", "loop", "muted", "controls"]
  = Audio

-- | \ 1.7.0.0 
type Audio = Element
    "audio"
    '[Flow, Phrasing, Embedded, Interactive, Palpable]
    ((Elements '["track", "source"] :|: Flow :|: Phrasing :|: Embedded :|: Interactive :|: Palpable) :&: NOT (Elements '["audio", "video"]))
    '["src", "crossorigin", "preload", "autoplay", "loop", "muted", "controls"]

-- | \ 4.8.11
data instance Element
    "track"
    '[]
    None
    '["kind", "src", "srclang", "label", "default"]
  = Track

-- | \ 1.7.0.0 
type Track = Element
    "track"
    '[]
    None
    '["kind", "src", "srclang", "label", "default"]

-- | \ 4.8.13
data instance Element
    "map"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["name"]
  = Map

-- | \ 1.7.0.0 
type Map = Element
    "map"
    '[Flow, Phrasing, Palpable]
    (Flow :|: Phrasing :|: Palpable)
    '["name"]

-- | \ 4.8.14
data instance Element
    "area"
    '[Flow, Phrasing]
    None
    '["alt", "coords", "shape", "href", "target", "download", "ping", "rel", "referrerpolicy"]
  = Area

-- | \ 1.7.0.0 
type Area = Element
    "area"
    '[Flow, Phrasing]
    None
    '["alt", "coords", "shape", "href", "target", "download", "ping", "rel", "referrerpolicy"]

-- | \ 4.9 Tabular data
--     4.9.1
data instance Element
    "table"
    '[Flow, Palpable]
    (Elements '["caption", "colgroup", "thead", "tbody", "tr", "tfoot"] :|: Scripting)
    '[]
  = Table

-- | \ 1.7.0.0 
type Table = Element
    "table"
    '[Flow, Palpable]
    (Elements '["caption", "colgroup", "thead", "tbody", "tr", "tfoot"] :|: Scripting)
    '[]

-- | \ 4.9.2
data instance Element
    "caption"
    '[]
    (Flow :|: NOT (Elements '["table"]))
    '[]
  = Caption

-- | \ 1.7.0.0 
type Caption = Element
    "caption"
    '[]
    (Flow :|: NOT (Elements '["table"]))
    '[]

-- | \ 4.9.3
data instance Element
    "colgroup"
    '[]
    (Elements '["col", "template"])
    '["span"]
  = Colgroup

-- | \ 1.7.0.0 
type Colgroup = Element
    "colgroup"
    '[]
    (Elements '["col", "template"])
    '["span"]

-- | \ 4.9.4
data instance Element
    "col"
    '[]
    None
    '["span"]
  = Col

-- | \ 1.7.0.0 
type Col = Element
    "col"
    '[]
    None
    '["span"]

-- | \ 4.9.5
data instance Element
    "tbody"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Tbody

-- | \ 1.7.0.0 
type Tbody = Element
    "tbody"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

-- | \ 4.9.6
data instance Element
    "thead"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Thead

-- | \ 1.7.0.0 
type Thead = Element
    "thead"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

-- | \ 4.9.7
data instance Element
    "tfoot"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]
  = Tfoot

-- | \ 1.7.0.0 
type Tfoot = Element
    "tfoot"
    '[]
    (Elements '["tr"] :|: Scripting)
    '[]

-- | \ 4.9.8
data instance Element
    "tr"
    '[]
    (Elements '["td", "th"] :|: Scripting)
    '[]
  = Tr

-- | \ 1.7.0.0 
type Tr = Element
    "tr"
    '[]
    (Elements '["td", "th"] :|: Scripting)
    '[]

-- | \ 4.9.9
data instance Element
    "td"
    '[]
    Flow
    '["colspan", "rowspan", "headers"]
  = Td

-- | \ 1.7.0.0 
type Td = Element
    "td"
    '[]
    Flow
    '["colspan", "rowspan", "headers"]

-- | \ 4.9.10
data instance Element
    "th"
    '[]
    (Flow :&: NOT (Elements '["header", "footer"] :|: Sectioning :|: Heading))
    '["colspan", "rowspan", "headers", "scope", "abbr"]
  = Th

-- | \ 1.7.0.0 
type Th = Element
    "th"
    '[]
    (Flow :&: NOT (Elements '["header", "footer"] :|: Sectioning :|: Heading))
    '["colspan", "rowspan", "headers", "scope", "abbr"]

-- | \ 4.10 Forms
--     4.10.3
data instance Element
    "form"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["form"]))
    '["acceptcharset", "action", "autocomplete", "enctype", "method", "name", "novalidate", "target", "rel"]
  = Form

-- | \ 1.7.0.0 
type Form = Element
    "form"
    '[Flow, Palpable]
    (Flow :&: NOT (Elements '["form"]))
    '["acceptcharset", "action", "autocomplete", "enctype", "method", "name", "novalidate", "target", "rel"]

-- | \ 4.10.4
data instance Element
    "label"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT (Elements '["label"]))
    '["for"]
  = Label

-- | \ 1.7.0.0 
type Label = Element
    "label"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT (Elements '["label"]))
    '["for"]

-- | \ 4.10.5
data instance Element
    "input"
    '[Flow, Phrasing, Interactive, Palpable]
    None
    '["accept", "alt", "autocomplete", "checked", "dirname", "disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "height", "list", "max", "maxlength", "min", "minlength", "multiple", "name", "pattern", "placeholder", "readonly", "required", "size", "src", "step", "type", "value", "width"]
  = Input

-- | \ 1.7.0.0 
type Input = Element
    "input"
    '[Flow, Phrasing, Interactive, Palpable]
    None
    '["accept", "alt", "autocomplete", "checked", "dirname", "disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "height", "list", "max", "maxlength", "min", "minlength", "multiple", "name", "pattern", "placeholder", "readonly", "required", "size", "src", "step", "type", "value", "width"]


-- | \ 4.10.6
data instance Element
    "button"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT Interactive)
    '["disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "name", "type", "value"]
  = Button

-- | \ 1.7.0.0 
type Button = Element
    "button"
    '[Flow, Phrasing, Interactive, Palpable]
    (Phrasing :&: NOT Interactive)
    '["disabled", "form", "formaction", "formenctype", "formmethod", "formnovalidate", "formtarget", "name", "type", "value"]

-- | \ 4.10.7
data instance Element
    "select"
    '[Flow, Phrasing, Interactive, Palpable]
    (Elements '["option", "optgroup"] :|: Scripting)
    '["autocomplete", "disabled", "form", "multiple", "name", "required", "size"]
  = Select

-- | \ 1.7.0.0 
type Select = Element
    "select"
    '[Flow, Phrasing, Interactive, Palpable]
    (Elements '["option", "optgroup"] :|: Scripting)
    '["autocomplete", "disabled", "form", "multiple", "name", "required", "size"]

-- | \ 4.10.8
data instance Element
    "datalist"
    '[Flow, Phrasing]
    (Phrasing :|: Scripting :|: Elements '["option"])
    '[]
  = Datalist

-- | \ 1.7.0.0 
type Datalist = Element
    "datalist"
    '[Flow, Phrasing]
    (Phrasing :|: Scripting :|: Elements '["option"])
    '[]

-- | \ 4.10.9
data instance Element
    "optgroup"
    '[]
    (Elements '["option"] :|: Scripting)
    '["disabled", "label"]
  = Optgroup

-- | \ 1.7.0.0 
type Optgroup = Element
    "optgroup"
    '[]
    (Elements '["option"] :|: Scripting)
    '["disabled", "label"]

-- | \ 4.10.10
data instance Element
    "option"
    '[]
    OnlyText
    '["disabled", "label", "selected", "value"]
  = Option

-- | \ 1.7.0.0 
type Option = Element
    "option"
    '[]
    OnlyText
    '["disabled", "label", "selected", "value"]

-- | \ 4.10.11
data instance Element
    "textarea"
    '[Flow, Phrasing, Interactive, Palpable]
    OnlyText
    '["autocomplete", "cols", "dirname", "disabled", "form", "maxlength", "minlength", "name", "placeholder", "readonly", "required", "rows", "wrap"]
  = Textarea

-- | \ 1.7.0.0 
type Textarea = Element
    "textarea"
    '[Flow, Phrasing, Interactive, Palpable]
    OnlyText
    '["autocomplete", "cols", "dirname", "disabled", "form", "maxlength", "minlength", "name", "placeholder", "readonly", "required", "rows", "wrap"]

-- | \ 4.10.12
data instance Element
    "output"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["for", "form", "name"]
  = Output

-- | \ 1.7.0.0 
type Output = Element
    "output"
    '[Flow, Phrasing, Palpable]
    Phrasing
    '["for", "form", "name"]

-- | \ 4.10.13
data instance Element
    "progress"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["progress"]))
    '["value", "max"]
  = Progress

-- | \ 1.7.0.0 
type Progress = Element
    "progress"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["progress"]))
    '["value", "max"]

-- | \ 4.10.14
data instance Element
    "meter"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["meter"]))
    '["value", "min", "max", "low", "high", "optimum"]
  = Meter

-- | \ 1.7.0.0 
type Meter = Element
    "meter"
    '[Flow, Phrasing, Palpable]
    (Phrasing :&: NOT (Elements '["meter"]))
    '["value", "min", "max", "low", "high", "optimum"]

-- | \ 4.10.15
data instance Element
    "fieldset"
    '[Flow, Palpable]
    (Elements '["legend"] :|: Flow)
    '["disabled", "form", "name"]
  = Fieldset

-- | \ 1.7.0.0 
type Fieldset = Element
    "fieldset"
    '[Flow, Palpable]
    (Elements '["legend"] :|: Flow)
    '["disabled", "form", "name"]

-- | \ 4.10.16
data instance Element
    "legend"
    '[]
    (Phrasing :|: Heading)
    '[]
  = Legend

-- | \ 1.7.0.0 
type Legend = Element
    "legend"
    '[]
    (Phrasing :|: Heading)
    '[]

-- | \ 4.11 Interactive elements
--     4.11.1
data instance Element
    "details"
    '[Flow, Interactive, Palpable]
    (Elements '["summary"] :|: Flow)
    '["open"]
  = Details

-- | \ 1.7.0.0 
type Details = Element
    "details"
    '[Flow, Interactive, Palpable]
    (Elements '["summary"] :|: Flow)
    '["open"]

-- | \ 4.11.2
data instance Element
    "summary"
    '[]
    (Phrasing :|: Heading)
    '[]
  = Summary

-- | \ 1.7.0.0 
type Summary = Element
    "summary"
    '[]
    (Phrasing :|: Heading)
    '[]

-- | \ 4.11.4
data instance Element
    "dialog"
    '[Flow]
    Flow
    '["open"]
  = Dialog

-- | \ 1.7.0.0 
type Dialog = Element
    "dialog"
    '[Flow]
    Flow
    '["open"]

-- | \ 4.12 Scripting
--     4.12.1
data instance Element
    "script"
    '[Metadata, Flow, Phrasing, Scripting]
    OnlyText
    '["src", "type", "nomodule", "async", "defer", "crossorigin", "integrity", "referrerpolicy"]
  = Script

-- | \ 1.7.0.0 
type Script = Element
    "script"
    '[Metadata, Flow, Phrasing, Scripting]
    OnlyText
    '["src", "type", "nomodule", "async", "defer", "crossorigin", "integrity", "referrerpolicy"]

-- | \ 4.12.2
data instance Element
    "noscript"
    '[Metadata, Flow, Phrasing]
    ((Elements '["link", "style", "meta"] :|: Metadata :|: Flow :|: Phrasing) :&: NOT (Elements '["noscript"]))
    '[]
  = Noscript

-- | \ 1.7.0.0 
type Noscript = Element
    "noscript"
    '[Metadata, Flow, Phrasing]
    ((Elements '["link", "style", "meta"] :|: Metadata :|: Flow :|: Phrasing) :&: NOT (Elements '["noscript"]))
    '[]

-- | \ 4.12.3
data instance Element
    "template"
    '[Metadata, Flow, Phrasing, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Palpable)
    '[]
  = Template

-- | \ 1.7.0.0 
type Template = Element
    "template"
    '[Metadata, Flow, Phrasing, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Palpable)
    '[]

-- | \ 4.12.4
data instance Element
    "slot"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["name"]
  = Slot

-- | \ 1.7.0.0 
type Slot = Element
    "slot"
    '[Flow, Phrasing]
    (Flow :|: Phrasing)
    '["name"]

-- | \ 4.12.5
data instance Element
    "canvas"
    '[Flow, Phrasing, Embedded, Palpable]
    (((Flow :|: Phrasing :|: Embedded :|: Palpable) :&: NOT Interactive) :|: Elements '["a", "img", "button", "input", "select"])
    '["width", "height"]
  = Canvas

-- | \ 1.7.0.0 
type Canvas = Element
    "canvas"
    '[Flow, Phrasing, Embedded, Palpable]
    (((Flow :|: Phrasing :|: Embedded :|: Palpable) :&: NOT Interactive) :|: Elements '["a", "img", "button", "input", "select"])
    '["width", "height"]

-- |
-- = Attributes

-- List of attributes (excluding event handler content attributes)
newtype instance                      Attribute "abbr"                      False v  = AbbrA v
type    AbbrA v =                     Attribute "abbr"                      False v
newtype instance                      Attribute "accept"                    False v  = AcceptA v
type    AcceptA v =                   Attribute "accept"                    False v
newtype instance                      Attribute "accept-charset"            False v  = AcceptCharsetA v
type    AcceptCharsetA v =            Attribute "accept-charset"            False v
newtype instance                      Attribute "accesskey"                 True  v  = AccesskeyA v
type    AccesskeyA v =                Attribute "accesskey"                 True  v
newtype instance                      Attribute "action"                    False v  = ActionA v
type    ActionA v =                   Attribute "action"                    False v
newtype instance                      Attribute "allow"                     False v  = AllowA v
type    AllowA v =                    Attribute "allow"                     False v
data    instance                      Attribute "allowfullscreen"           False () = AllowfullscreenA
type    AllowfullscreenA =            Attribute "allowfullscreen"           False ()
newtype instance                      Attribute "alt"                       False v  = AltA v
type    AltA v =                      Attribute "alt"                       False v 
newtype instance                      Attribute "as"                        False v  = AsA v
type    AsA v =                       Attribute "as"                        False v 
data    instance                      Attribute "async"                     False () = AsyncA
type    AsyncA =                      Attribute "async"                     False ()
newtype instance                      Attribute "autocapitalize"            True  v  = AutocapitalizeA v
type    AutocapitalizeA v =           Attribute "autocapitalize"            True  v 
newtype instance                      Attribute "autocomplete"              False v  = AutocompleteA v
type    AutocompleteA v =             Attribute "autocomplete"              False v 
data    instance                      Attribute "autofocus"                 True  () = AutofocusA
type    AutofocusA =                  Attribute "autofocus"                 True  ()
data    instance                      Attribute "autoplay"                  False () = AutoplayA
type    AutoplayA =                   Attribute "autoplay"                  False ()
newtype instance                      Attribute "charset"                   False v  = CharsetA v
type    CharsetA v =                  Attribute "charset"                   False v
data    instance                      Attribute "checked"                   False () = CheckedA
type    CheckedA =                    Attribute "checked"                   False ()
newtype instance                      Attribute "cite"                      False v  = CiteA v
type    CiteA v =                     Attribute "cite"                      False v 
newtype instance                      Attribute "class"                     True  v  = ClassA v
type    ClassA v =                    Attribute "class"                     True  v 
newtype instance                      Attribute "color"                     False v  = ColorA v
type    ColorA v =                    Attribute "color"                     False v 
newtype instance                      Attribute "cols"                      False v  = ColsA v
type    ColsA v =                     Attribute "cols"                      False v 
newtype instance                      Attribute "colspan"                   False v  = ColspanA v
type    ColspanA v =                  Attribute "colspan"                   False v 
newtype instance                      Attribute "content"                   False v  = ContentA v
type    ContentA v =                  Attribute "content"                   False v 
newtype instance                      Attribute "contenteditable"           True  v  = ContenteditableA v
type    ContenteditableA v =          Attribute "contenteditable"           True  v 
data    instance                      Attribute "controls"                  False () = ControlsA
type    ControlsA =                   Attribute "controls"                  False ()
newtype instance                      Attribute "coords"                    False v  = CoordsA v
type    CoordsA v =                   Attribute "coords"                    False v 
newtype instance                      Attribute "crossorigin"               False v  = CrossoriginA v
type    CrossoriginA v =              Attribute "crossorigin"               False v 
newtype instance                      Attribute "data"                      False v  = DataA v
type    DataA v =                     Attribute "data"                      False v 
newtype instance                      Attribute "datetime"                  False v  = DatetimeA v
type    DatetimeA v =                 Attribute "datetime"                  False v 
newtype instance                      Attribute "decoding"                  False v  = DecodingA v
type    DecodingA v =                 Attribute "decoding"                  False v 
data    instance                      Attribute "default"                   False () = DefaultA
type    DefaultA =                    Attribute "default"                   False ()
data    instance                      Attribute "defer"                     False () = DeferA
type    DeferA =                      Attribute "defer"                     False ()
newtype instance                      Attribute "dir"                       True  v  = DirA v
type    DirA v =                      Attribute "dir"                       True  v 
newtype instance                      Attribute "dirname"                   False v  = DirnameA v
type    DirnameA v =                  Attribute "dirname"                   False v 
data    instance                      Attribute "disabled"                  False () = DisabledA
type    DisabledA =                   Attribute "disabled"                  False ()
newtype instance                      Attribute "download"                  False v  = DownloadA v
type    DownloadA v =                 Attribute "download"                  False v 
newtype instance                      Attribute "draggable"                 True  v  = DraggableA v
type    DraggableA v =                Attribute "draggable"                 True  v 
newtype instance                      Attribute "enctype"                   False v  = EnctypeA v
type    EnctypeA v =                  Attribute "enctype"                   False v 
newtype instance                      Attribute "enterkeyhint"              True  v  = EnterkeyhintA v
type    EnterkeyhintA v =             Attribute "enterkeyhint"              True  v 
newtype instance                      Attribute "for"                       False v  = ForA v
type    ForA v =                      Attribute "for"                       False v 
newtype instance                      Attribute "form"                      False v  = FormA v
type    FormA v =                     Attribute "form"                      False v 
newtype instance                      Attribute "formaction"                False v  = FormactionA v
type    FormactionA v =               Attribute "formaction"                False v 
newtype instance                      Attribute "formenctype"               False v  = FormenctypeA v
type    FormenctypeA v =              Attribute "formenctype"               False v 
newtype instance                      Attribute "formmethod"                False v  = FormmethodA v
type    FormmethodA v =               Attribute "formmethod"                False v 
data    instance                      Attribute "formnovalidate"            False () = FormnovalidateA
type    FormnovalidateA =             Attribute "formnovalidate"            False ()
newtype instance                      Attribute "formtarget"                False v  = FormtargetA v
type    FormtargetA v =               Attribute "formtarget"                False v 
newtype instance                      Attribute "headers"                   False v  = HeadersA v
type    HeadersA v =                  Attribute "headers"                   False v 
newtype instance                      Attribute "height"                    False v  = HeightA v
type    HeightA v =                   Attribute "height"                    False v 
data    instance                      Attribute "hidden"                    True  () = HiddenA
type    HiddenA =                     Attribute "hidden"                    True  ()
newtype instance                      Attribute "high"                      False v  = HighA v
type    HighA v =                     Attribute "high"                      False v 
newtype instance                      Attribute "href"                      False v  = HrefA v
type    HrefA v =                     Attribute "href"                      False v 
newtype instance                      Attribute "hreflang"                  False v  = HreflangA v
type    HreflangA v =                 Attribute "hreflang"                  False v 
newtype instance                      Attribute "httpEquiv"                 False v  = HttpEquivA v
type    HttpEquivA v =                Attribute "httpEquiv"                 False v 
newtype instance                      Attribute "id"                        True  v  = IdA v
type    IdA v =                       Attribute "id"                        True  v 
newtype instance                      Attribute "imagesizes"                False v  = ImagesizesA v
type    ImagesizesA v =               Attribute "imagesizes"                False v 
newtype instance                      Attribute "imagesrcset"               False v  = ImagesrcsetA v
type    ImagesrcsetA v =              Attribute "imagesrcset"               False v 
newtype instance                      Attribute "inputmode"                 True  v  = InputmodeA v
type    InputmodeA v =                Attribute "inputmode"                 True  v 
newtype instance                      Attribute "integrity"                 False v  = IntegrityA v
type    IntegrityA v =                Attribute "integrity"                 False v 
newtype instance                      Attribute "is"                        True  v  = IsA v
type    IsA v =                       Attribute "is"                        True  v 
data    instance                      Attribute "ismap"                     False () = IsmapA
type    IsmapA =                      Attribute "ismap"                     False ()
newtype instance                      Attribute "itemid"                    True  v  = ItemidA v
type    ItemidA v =                   Attribute "itemid"                    True  v 
newtype instance                      Attribute "itemprop"                  True  v  = ItempropA v
type    ItempropA v =                 Attribute "itemprop"                  True  v 
newtype instance                      Attribute "itemref"                   True  v  = ItemrefA v
type    ItemrefA v =                  Attribute "itemref"                   True  v 
data    instance                      Attribute "itemscope"                 True  () = ItemscopeA
type    ItemscopeA =                  Attribute "itemscope"                 True  ()
newtype instance                      Attribute "itemtype"                  True  v  = ItemtypeA v
type    ItemtypeA v =                 Attribute "itemtype"                  True  v 
newtype instance                      Attribute "kind"                      False v  = KindA v
type    KindA v =                     Attribute "kind"                      False v 
newtype instance                      Attribute "label"                     False v  = LabelA v
type    LabelA v =                    Attribute "label"                     False v 
newtype instance                      Attribute "lang"                      True  v  = LangA v
type    LangA v =                     Attribute "lang"                      True  v 
newtype instance                      Attribute "list"                      False v  = ListA v
type    ListA v =                     Attribute "list"                      False v 
newtype instance                      Attribute "loading"                   False v  = LoadingA v
type    LoadingA v =                  Attribute "loading"                   False v 
data    instance                      Attribute "loop"                      False () = LoopA
type    LoopA =                       Attribute "loop"                      False ()
newtype instance                      Attribute "low"                       False v  = LowA v
type    LowA v =                      Attribute "low"                       False v 
newtype instance                      Attribute "manifest"                  False v  = ManifestA v
type    ManifestA v =                 Attribute "manifest"                  False v 
newtype instance                      Attribute "max"                       False v  = MaxA v
type    MaxA v =                      Attribute "max"                       False v 
newtype instance                      Attribute "maxlength"                 False v  = MaxlengthA v
type    MaxlengthA v =                Attribute "maxlength"                 False v 
newtype instance                      Attribute "media"                     False v  = MediaA v
type    MediaA v =                    Attribute "media"                     False v 
newtype instance                      Attribute "method"                    False v  = MethodA v
type    MethodA v =                   Attribute "method"                    False v 
newtype instance                      Attribute "min"                       False v  = MinA v
type    MinA v =                      Attribute "min"                       False v 
newtype instance                      Attribute "minlength"                 False v  = MinlengthA v
type    MinlengthA v =                Attribute "minlength"                 False v 
data    instance                      Attribute "multiple"                  False () = MultipleA
type    MultipleA =                   Attribute "multiple"                  False ()
data    instance                      Attribute "muted"                     False () = MutedA
type    MutedA =                      Attribute "muted"                     False ()
newtype instance                      Attribute "name"                      False v  = NameA v
type    NameA v =                     Attribute "name"                      False v 
data    instance                      Attribute "nomodule"                  False () = NomoduleA
type    NomoduleA =                   Attribute "nomodule"                  False ()
newtype instance                      Attribute "nonce"                     True  v  = NonceA v
type    NonceA v =                    Attribute "nonce"                     True  v 
data    instance                      Attribute "novalidate"                False () = NovalidateA
type    NovalidateA =                 Attribute "novalidate"                False ()
data    instance                      Attribute "open"                      False () = OpenA
type    OpenA =                       Attribute "open"                      False ()
newtype instance                      Attribute "optimum"                   False v  = OptimumA v
type    OptimumA v =                  Attribute "optimum"                   False v 
newtype instance                      Attribute "pattern"                   False v  = PatternA v
type    PatternA v =                  Attribute "pattern"                   False v 
newtype instance                      Attribute "ping"                      False v  = PingA v
type    PingA v =                     Attribute "ping"                      False v 
newtype instance                      Attribute "placeholder"               False v  = PlaceholderA v
type    PlaceholderA v =              Attribute "placeholder"               False v 
data    instance                      Attribute "playsinline"               False () = PlaysinlineA
type    PlaysinlineA =                Attribute "playsinline"               False ()
newtype instance                      Attribute "poster"                    False v  = PosterA v
type    PosterA v =                   Attribute "poster"                    False v 
newtype instance                      Attribute "preload"                   False v  = PreloadA v
type    PreloadA v =                  Attribute "preload"                   False v 
data    instance                      Attribute "readonly"                  False () = ReadonlyA
type    ReadonlyA =                   Attribute "readonly"                  False ()
newtype instance                      Attribute "referrerpolicy"            False v  = ReferrerpolicyA v
type    ReferrerpolicyA v =           Attribute "referrerpolicy"            False v 
newtype instance                      Attribute "rel"                       False v  = RelA v
type    RelA v =                      Attribute "rel"                       False v 
data    instance                      Attribute "required"                  False () = RequiredA
type    RequiredA =                   Attribute "required"                  False ()
data    instance                      Attribute "reversed"                  False () = ReversedA
type    ReversedA =                   Attribute "reversed"                  False ()
newtype instance                      Attribute "rows"                      False v  = RowsA v
type    RowsA v =                     Attribute "rows"                      False v 
newtype instance                      Attribute "rowspan"                   False v  = RowspanA v
type    RowspanA v =                  Attribute "rowspan"                   False v 
newtype instance                      Attribute "sandbox"                   False v  = SandboxA v
type    SandboxA v =                  Attribute "sandbox"                   False v 
newtype instance                      Attribute "scope"                     False v  = ScopeA v
type    ScopeA v =                    Attribute "scope"                     False v 
data    instance                      Attribute "selected"                  False () = SelectedA
type    SelectedA =                   Attribute "selected"                  False ()
newtype instance                      Attribute "shape"                     False v  = ShapeA v
type    ShapeA v =                    Attribute "shape"                     False v 
newtype instance                      Attribute "size"                      False v  = SizeA v
type    SizeA v =                     Attribute "size"                      False v 
newtype instance                      Attribute "sizes"                     False v  = SizesA v
type    SizesA v =                    Attribute "sizes"                     False v 
newtype instance                      Attribute "slot"                      True  v  = SlotA v
type    SlotA v =                     Attribute "slot"                      True  v 
newtype instance                      Attribute "span"                      False v  = SpanA v
type    SpanA v =                     Attribute "span"                      False v 
newtype instance                      Attribute "spellcheck"                True  v  = SpellcheckA v
type    SpellcheckA v =               Attribute "spellcheck"                True  v 
newtype instance                      Attribute "src"                       False v  = SrcA v
type    SrcA v =                      Attribute "src"                       False v 
newtype instance                      Attribute "srcdoc"                    False v  = SrcdocA v
type    SrcdocA v =                   Attribute "srcdoc"                    False v 
newtype instance                      Attribute "srclang"                   False v  = SrclangA v
type    SrclangA v =                  Attribute "srclang"                   False v 
newtype instance                      Attribute "srcset"                    False v  = SrcsetA v
type    SrcsetA v =                   Attribute "srcset"                    False v 
newtype instance                      Attribute "start"                     False v  = StartA v
type    StartA v =                    Attribute "start"                     False v 
newtype instance                      Attribute "step"                      False v  = StepA v
type    StepA v =                     Attribute "step"                      False v 
newtype instance                      Attribute "style"                     True  v  = StyleA v
type    StyleA v =                    Attribute "style"                     True  v 
newtype instance                      Attribute "tabindex"                  True  v  = TabindexA v
type    TabindexA v =                 Attribute "tabindex"                  True  v 
newtype instance                      Attribute "target"                    False v  = TargetA v
type    TargetA v =                   Attribute "target"                    False v 
newtype instance                      Attribute "title"                     True  v  = TitleA v
type    TitleA v =                    Attribute "title"                     True  v 
newtype instance                      Attribute "translate"                 True  v  = TranslateA v
type    TranslateA v =                Attribute "translate"                 True  v 
newtype instance                      Attribute "type"                      False v  = TypeA v
type    TypeA v =                     Attribute "type"                      False v 
newtype instance                      Attribute "usemap"                    False v  = UsemapA v
type    UsemapA v =                   Attribute "usemap"                    False v 
newtype instance                      Attribute "value"                     False v  = ValueA v
type    ValueA v =                    Attribute "value"                     False v 
newtype instance                      Attribute "width"                     False v  = WidthA v
type    WidthA v =                    Attribute "width"                     False v 
newtype instance                      Attribute "wrap"                      False v  = WrapA v
type    WrapA v =                     Attribute "wrap"                      False v 

-- List of event handler content attributes
newtype instance                       Attribute "onabort"                   True  v  = OnabortA v
type    OnabortA v =                   Attribute "onabort"                   True  v 
newtype instance                       Attribute "onauxclick"                True  v  = OnauxclickA v
type    OnauxclickA v =                Attribute "onauxclick"                True  v 
newtype instance                       Attribute "onafterprint"              False v  = OnafterprintA v
type    OnafterprintA v =              Attribute "onafterprint"              False v 
newtype instance                       Attribute "onbeforeprint"             False v  = OnbeforeprintA v
type    OnbeforeprintA v =             Attribute "onbeforeprint"             False v 
newtype instance                       Attribute "onbeforeunload"            False v  = OnbeforeunloadA v
type    OnbeforeunloadA v =            Attribute "onbeforeunload"            False v 
newtype instance                       Attribute "onblur"                    True  v  = OnblurA v
type    OnblurA v =                    Attribute "onblur"                    True  v 
newtype instance                       Attribute "oncancel"                  True  v  = OncancelA v
type    OncancelA v =                  Attribute "oncancel"                  True  v 
newtype instance                       Attribute "oncanplay"                 True  v  = OncanplayA v
type    OncanplayA v =                 Attribute "oncanplay"                 True  v 
newtype instance                       Attribute "oncanplaythrough"          True  v  = OncanplaythroughA v
type    OncanplaythroughA v =          Attribute "oncanplaythrough"          True  v 
newtype instance                       Attribute "onchange"                  True  v  = OnchangeA v
type    OnchangeA v =                  Attribute "onchange"                  True  v 
newtype instance                       Attribute "onclick"                   True  v  = OnclickA v
type    OnclickA v =                   Attribute "onclick"                   True  v 
newtype instance                       Attribute "onclose"                   True  v  = OncloseA v
type    OncloseA v =                   Attribute "onclose"                   True  v 
newtype instance                       Attribute "oncontextmenu"             True  v  = OncontextmenuA v
type    OncontextmenuA v =             Attribute "oncontextmenu"             True  v 
newtype instance                       Attribute "oncopy"                    True  v  = OncopyA v
type    OncopyA v =                    Attribute "oncopy"                    True  v 
newtype instance                       Attribute "oncuechange"               True  v  = OncuechangeA v
type    OncuechangeA v =               Attribute "oncuechange"               True  v 
newtype instance                       Attribute "oncut"                     True  v  = OncutA v
type    OncutA v =                     Attribute "oncut"                     True  v 
newtype instance                       Attribute "ondblclick"                True  v  = OndblclickA v
type    OndblclickA v =                Attribute "ondblclick"                True  v 
newtype instance                       Attribute "ondrag"                    True  v  = OndragA v
type    OndragA v =                    Attribute "ondrag"                    True  v 
newtype instance                       Attribute "ondragend"                 True  v  = OndragendA v
type    OndragendA v =                 Attribute "ondragend"                 True  v 
newtype instance                       Attribute "ondragenter"               True  v  = OndragenterA v
type    OndragenterA v =               Attribute "ondragenter"               True  v 
newtype instance                       Attribute "ondragleave"               True  v  = OndragleaveA v
type    OndragleaveA v =               Attribute "ondragleave"               True  v 
newtype instance                       Attribute "ondragover"                True  v  = OndragoverA v
type    OndragoverA v =                Attribute "ondragover"                True  v 
newtype instance                       Attribute "ondragstart"               True  v  = OndragstartA v
type    OndragstartA v =               Attribute "ondragstart"               True  v 
newtype instance                       Attribute "ondrop"                    True  v  = OndropA v
type    OndropA v =                    Attribute "ondrop"                    True  v 
newtype instance                       Attribute "ondurationchange"          True  v  = OndurationchangeA v
type    OndurationchangeA v =          Attribute "ondurationchange"          True  v 
newtype instance                       Attribute "onemptied"                 True  v  = OnemptiedA v
type    OnemptiedA v =                 Attribute "onemptied"                 True  v 
newtype instance                       Attribute "onended"                   True  v  = OnendedA v
type    OnendedA v =                   Attribute "onended"                   True  v 
newtype instance                       Attribute "onerror"                   True  v  = OnerrorA v
type    OnerrorA v =                   Attribute "onerror"                   True  v 
newtype instance                       Attribute "onfocus"                   True  v  = OnfocusA v
type    OnfocusA v =                   Attribute "onfocus"                   True  v 
newtype instance                       Attribute "onformdata"                True  v  = OnformdataA v
type    OnformdataA v =                Attribute "onformdata"                True  v 
newtype instance                       Attribute "onhashchange"              False v  = OnhashchangeA v
type    OnhashchangeA v =              Attribute "onhashchange"              False v 
newtype instance                       Attribute "oninput"                   True  v  = OninputA v
type    OninputA v =                   Attribute "oninput"                   True  v 
newtype instance                       Attribute "oninvalid"                 True  v  = OninvalidA v
type    OninvalidA v =                 Attribute "oninvalid"                 True  v 
newtype instance                       Attribute "onkeydown"                 True  v  = OnkeydownA v
type    OnkeydownA v =                 Attribute "onkeydown"                 True  v 
newtype instance                       Attribute "onkeypress"                True  v  = OnkeypressA v
type    OnkeypressA v =                Attribute "onkeypress"                True  v 
newtype instance                       Attribute "onkeyup"                   True  v  = OnkeyupA v
type    OnkeyupA v =                   Attribute "onkeyup"                   True  v 
newtype instance                       Attribute "onlanguagechange"          False v  = OnlanguagechangeA v
type    OnlanguagechangeA v =          Attribute "onlanguagechange"          False v 
newtype instance                       Attribute "onload"                    True  v  = OnloadA v
type    OnloadA v =                    Attribute "onload"                    True  v 
newtype instance                       Attribute "onloadeddata"              True  v  = OnloadeddataA v
type    OnloadeddataA v =              Attribute "onloadeddata"              True  v 
newtype instance                       Attribute "onloadedmetadata"          True  v  = OnloadedmetadataA v
type    OnloadedmetadataA v =          Attribute "onloadedmetadata"          True  v 
newtype instance                       Attribute "onloadstart"               True  v  = OnloadstartA v
type    OnloadstartA v =               Attribute "onloadstart"               True  v 
newtype instance                       Attribute "onmessage"                 False v  = OnmessageA v
type    OnmessageA v =                 Attribute "onmessage"                 False v 
newtype instance                       Attribute "onmessageerror"            False v  = OnmessageerrorA v
type    OnmessageerrorA v =            Attribute "onmessageerror"            False v 
newtype instance                       Attribute "onmousedown"               True  v  = OnmousedownA v
type    OnmousedownA v =               Attribute "onmousedown"               True  v 
newtype instance                       Attribute "onmouseenter"              True  v  = OnmouseenterA v
type    OnmouseenterA v =              Attribute "onmouseenter"              True  v 
newtype instance                       Attribute "onmouseleave"              True  v  = OnmouseleaveA v
type    OnmouseleaveA v =              Attribute "onmouseleave"              True  v 
newtype instance                       Attribute "onmousemove"               True  v  = OnmousemoveA v
type    OnmousemoveA v =               Attribute "onmousemove"               True  v 
newtype instance                       Attribute "onmouseout"                True  v  = OnmouseoutA v
type    OnmouseoutA v =                Attribute "onmouseout"                True  v 
newtype instance                       Attribute "onmouseover"               True  v  = OnmouseoverA v
type    OnmouseoverA v =               Attribute "onmouseover"               True  v 
newtype instance                       Attribute "onmouseup"                 True  v  = OnmouseupA v
type    OnmouseupA v =                 Attribute "onmouseup"                 True  v 
newtype instance                       Attribute "onoffline"                 False v  = OnofflineA v
type    OnofflineA v =                 Attribute "onoffline"                 False v 
newtype instance                       Attribute "ononline"                  False v  = OnonlineA v
type    OnonlineA v =                  Attribute "ononline"                  False v 
newtype instance                       Attribute "onpagehide"                False v  = OnpagehideA v
type    OnpagehideA v =                Attribute "onpagehide"                False v 
newtype instance                       Attribute "onpageshow"                False v  = OnpageshowA v
type    OnpageshowA v =                Attribute "onpageshow"                False v 
newtype instance                       Attribute "onpaste"                   True  v  = OnpasteA v
type    OnpasteA v =                   Attribute "onpaste"                   True  v 
newtype instance                       Attribute "onpause"                   True  v  = OnpauseA v
type    OnpauseA v =                   Attribute "onpause"                   True  v 
newtype instance                       Attribute "onplay"                    True  v  = OnplayA v
type    OnplayA v =                    Attribute "onplay"                    True  v 
newtype instance                       Attribute "onplaying"                 True  v  = OnplayingA v
type    OnplayingA v =                 Attribute "onplaying"                 True  v 
newtype instance                       Attribute "onpopstate"                False v  = OnpopstateA v
type    OnpopstateA v =                Attribute "onpopstate"                False v 
newtype instance                       Attribute "onprogress"                True  v  = OnprogressA v
type    OnprogressA v =                Attribute "onprogress"                True  v 
newtype instance                       Attribute "onratechange"              True  v  = OnratechangeA v
type    OnratechangeA v =              Attribute "onratechange"              True  v 
newtype instance                       Attribute "onreset"                   True  v  = OnresetA v
type    OnresetA v =                   Attribute "onreset"                   True  v 
newtype instance                       Attribute "onresize"                  True  v  = OnresizeA v
type    OnresizeA v =                  Attribute "onresize"                  True  v 
newtype instance                       Attribute "onrejectionhandled"        False v  = OnrejectionhandledA v
type    OnrejectionhandledA v =        Attribute "onrejectionhandled"        False v 
newtype instance                       Attribute "onscroll"                  True  v  = OnscrollA v
type    OnscrollA v =                  Attribute "onscroll"                  True  v 
newtype instance                       Attribute "onsecuritypolicyviolation" True  v  = OnsecuritypolicyviolationA v
type    OnsecuritypolicyviolationA v = Attribute "onsecuritypolicyviolation" True  v 
newtype instance                       Attribute "onseeked"                  True  v  = OnseekedA v
type    OnseekedA v =                  Attribute "onseeked"                  True  v 
newtype instance                       Attribute "onseeking"                 True  v  = OnseekingA v
type    OnseekingA v =                 Attribute "onseeking"                 True  v 
newtype instance                       Attribute "onselect"                  True  v  = OnselectA v
type    OnselectA v =                  Attribute "onselect"                  True  v 
newtype instance                       Attribute "onslotchange"              True  v  = OnslotchangeA v
type    OnslotchangeA v =              Attribute "onslotchange"              True  v 
newtype instance                       Attribute "onstalled"                 True  v  = OnstalledA v
type    OnstalledA v =                 Attribute "onstalled"                 True  v 
newtype instance                       Attribute "onstorage"                 False v  = OnstorageA v
type    OnstorageA v =                 Attribute "onstorage"                 False v 
newtype instance                       Attribute "onsubmit"                  True  v  = OnsubmitA v
type    OnsubmitA v =                  Attribute "onsubmit"                  True  v 
newtype instance                       Attribute "onsuspend"                 True  v  = OnsuspendA v
type    OnsuspendA v =                 Attribute "onsuspend"                 True  v 
newtype instance                       Attribute "ontimeupdate"              True  v  = OntimeupdateA v
type    OntimeupdateA v =              Attribute "ontimeupdate"              True  v 
newtype instance                       Attribute "ontoggle"                  True  v  = OntoggleA v
type    OntoggleA v =                  Attribute "ontoggle"                  True  v 
newtype instance                       Attribute "onunhandledrejection"      False v  = OnunhandledrejectionA v
type    OnunhandledrejectionA v =      Attribute "onunhandledrejection"      False v 
newtype instance                       Attribute "onunload"                  False v  = OnunloadA v
type    OnunloadA v =                  Attribute "onunload"                  False v 
newtype instance                       Attribute "onvolumechange"            True  v  = OnvolumechangeA v
type    OnvolumechangeA v =            Attribute "onvolumechange"            True  v 
newtype instance                       Attribute "onwaiting"                 True  v  = OnwaitingA v
type    OnwaitingA v =                 Attribute "onwaiting"                 True  v 
newtype instance                       Attribute "onwheel"                   True  v  = OnwheelA v
