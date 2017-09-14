{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE GADTs                      #-}

module Html.Type where

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
  | Data'A
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
  | LoopA
  | LowA
  | ManifestA
  | MaxA
  | MaxlengthA
  | MinlengthA
  | MediaA
  | MethodA
  | MinA
  | MultipleA
  | MutedA
  | NameA
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
  | RelA
  | RequiredA
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
  | TypeA
  | UsemapA
  | ValueA
  | WidthA
  | WrapA

newtype (:=) (a :: Attribute) b = AT b

-- | Check whether `b` is a valid child of `a`.  You'll propably never
-- need to call this directly.  Through a GADT, it is enforced that
-- every child is lawful.
type family (a :: Element) ?> b :: Constraint where
  a ?> (b # c)         = (a ?> b, a ?> c)
  a ?> (b > _)         = MaybeTypeError a b (TestPaternity (SingleElement b) (GetInfo a) (GetInfo b))
  a ?> (b :@: _) _     = MaybeTypeError a b (TestPaternity (SingleElement b) (GetInfo a) (GetInfo b))
  a ?> Maybe b         = a ?> b
  a ?> Either b c      = (a ?> b, a ?> c)
  a ?> f (b > c)       = a ?> (b > c)
  a ?> f ((b :@: c) d) = a ?> (b > d)
  a ?> f (b # c)       = a ?> (b # c)
  a ?> ()              = ()
  a ?> (b -> c)        = TypeError (Text "Html elements can't contain functions")
  a ?> b               = CheckString a b

type family Null xs where
  Null '[] = True
  Null _ = False

type family (a :: Element) ??> b :: Constraint where
  a ??> (b # c)  = (a ??> b, a ??> c)
  a ??> (b := _) = If (Elem a (GetAttributeInfo b) || Null (GetAttributeInfo b))
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

-- | Descend to a valid child of an element.
-- It is recommended to use the predefined elements.
--
-- >>> Child "a" :: 'Div > String
-- <div>a</div>
--
-- >>> div_ "a"
-- <div>a</div>
data (>) (a :: Element) b where
  Child :: (a ?> b) => b -> a > b
infixr 8 >

-- | Decorate an element with attributes and descend to a valid child.
--
-- >>> WithAttributes (A.class_ "bar") "a" :: 'Div :> String
-- <div class="bar">a</div>
data (:@:) (a :: Element) b c where
  WithAttributes :: (a ??> b, a ?> c) => b -> c -> (a :@: b) c
infixr 8 :@:

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw a

  -------------------
  -- internal code --
  -------------------

type family ShowElement e where
  ShowElement DOCTYPE    = "!DOCTYPE html"
  ShowElement A          = "a"
  ShowElement Abbr       = "abbr"
  ShowElement Acronym    = "acronym"
  ShowElement Address    = "address"
  ShowElement Applet     = "applet"
  ShowElement Area       = "area"
  ShowElement Article    = "article"
  ShowElement Aside      = "aside"
  ShowElement Audio      = "audio"
  ShowElement B          = "b"
  ShowElement Base       = "base"
  ShowElement Basefont   = "basefont"
  ShowElement Bdi        = "bdi"
  ShowElement Bdo        = "bdo"
  ShowElement Bgsound    = "bgsound"
  ShowElement Big        = "big"
  ShowElement Blink      = "blink"
  ShowElement Blockquote = "blockquote"
  ShowElement Body       = "body"
  ShowElement Br         = "br"
  ShowElement Button     = "button"
  ShowElement Canvas     = "canvas"
  ShowElement Caption    = "caption"
  ShowElement Center     = "center"
  ShowElement Cite       = "cite"
  ShowElement Code       = "code"
  ShowElement Col        = "col"
  ShowElement Colgroup   = "colgroup"
  ShowElement Command    = "command"
  ShowElement Content    = "content"
  ShowElement Data       = "data"
  ShowElement Datalist   = "datalist"
  ShowElement Dd         = "dd"
  ShowElement Del        = "del"
  ShowElement Details    = "details"
  ShowElement Dfn        = "dfn"
  ShowElement Dialog     = "dialog"
  ShowElement Dir        = "dir"
  ShowElement Div        = "div"
  ShowElement Dl         = "dl"
  ShowElement Dt         = "dt"
  ShowElement 'Element   = "element"
  ShowElement Em         = "em"
  ShowElement Embed      = "embed"
  ShowElement Fieldset   = "fieldset"
  ShowElement Figcaption = "figcaption"
  ShowElement Figure     = "figure"
  ShowElement Font       = "font"
  ShowElement Footer     = "footer"
  ShowElement Form       = "form"
  ShowElement Frame      = "frame"
  ShowElement Frameset   = "frameset"
  ShowElement H1         = "h1"
  ShowElement H2         = "h2"
  ShowElement H3         = "h3"
  ShowElement H4         = "h4"
  ShowElement H5         = "h5"
  ShowElement H6         = "h6"
  ShowElement Head       = "head"
  ShowElement Header     = "header"
  ShowElement Hgroup     = "hgroup"
  ShowElement Hr         = "hr"
  ShowElement Html       = "html"
  ShowElement I          = "i"
  ShowElement Iframe     = "iframe"
  ShowElement Image      = "image"
  ShowElement Img        = "img"
  ShowElement Input      = "input"
  ShowElement Ins        = "ins"
  ShowElement Isindex    = "isindex"
  ShowElement Kbd        = "kbd"
  ShowElement Keygen     = "keygen"
  ShowElement Label      = "label"
  ShowElement Legend     = "legend"
  ShowElement Li         = "li"
  ShowElement Link       = "link"
  ShowElement Listing    = "listing"
  ShowElement Main       = "main"
  ShowElement Map        = "map"
  ShowElement Mark       = "mark"
  ShowElement Marquee    = "marquee"
  ShowElement Math       = "math"
  ShowElement Menu       = "menu"
  ShowElement Menuitem   = "menuitem"
  ShowElement Meta       = "meta"
  ShowElement Meter      = "meter"
  ShowElement Multicol   = "multicol"
  ShowElement Nav        = "nav"
  ShowElement Nextid     = "nextid"
  ShowElement Nobr       = "nobr"
  ShowElement Noembed    = "noembed"
  ShowElement Noframes   = "noframes"
  ShowElement Noscript   = "noscript"
  ShowElement Object     = "object"
  ShowElement Ol         = "ol"
  ShowElement Optgroup   = "optgroup"
  ShowElement Option     = "option"
  ShowElement Output     = "output"
  ShowElement P          = "p"
  ShowElement Param      = "param"
  ShowElement Picture    = "picture"
  ShowElement Plaintext  = "plaintext"
  ShowElement Pre        = "pre"
  ShowElement Progress   = "progress"
  ShowElement Q          = "q"
  ShowElement Rp         = "rp"
  ShowElement Rt         = "rt"
  ShowElement Rtc        = "rtc"
  ShowElement Ruby       = "ruby"
  ShowElement S          = "s"
  ShowElement Samp       = "samp"
  ShowElement Script     = "script"
  ShowElement Section    = "section"
  ShowElement Select     = "select"
  ShowElement Shadow     = "shadow"
  ShowElement Slot       = "slot"
  ShowElement Small      = "small"
  ShowElement Source     = "source"
  ShowElement Spacer     = "spacer"
  ShowElement Span       = "span"
  ShowElement Strike     = "strike"
  ShowElement Strong     = "strong"
  ShowElement Style      = "style"
  ShowElement Sub        = "sub"
  ShowElement Summary    = "summary"
  ShowElement Sup        = "sup"
  ShowElement Svg        = "svg"
  ShowElement Table      = "table"
  ShowElement Tbody      = "tbody"
  ShowElement Td         = "td"
  ShowElement Template   = "template"
  ShowElement Textarea   = "textarea"
  ShowElement Tfoot      = "tfoot"
  ShowElement Th         = "th"
  ShowElement Thead      = "thead"
  ShowElement Time       = "time"
  ShowElement Title      = "title"
  ShowElement Tr         = "tr"
  ShowElement Track      = "track"
  ShowElement Tt         = "tt"
  ShowElement U          = "u"
  ShowElement Ul         = "ul"
  ShowElement Var        = "var"
  ShowElement Video      = "video"
  ShowElement Wbr        = "wbr"
  ShowElement Xmp        = "xmp"

type family ShowAttribute (x :: Attribute) where
  ShowAttribute AcceptA          = "accept"
  ShowAttribute AcceptCharsetA   = "accept-charset"
  ShowAttribute AccesskeyA       = "accesskey"
  ShowAttribute ActionA          = "action"
  ShowAttribute AlignA           = "align"
  ShowAttribute AltA             = "alt"
  ShowAttribute AsyncA           = "async"
  ShowAttribute AutocompleteA    = "autocomplete"
  ShowAttribute AutofocusA       = "autofocus"
  ShowAttribute AutoplayA        = "autoplay"
  ShowAttribute AutosaveA        = "autosave"
  ShowAttribute BgcolorA         = "bgcolor"
  ShowAttribute BorderA          = "border"
  ShowAttribute BufferedA        = "buffered"
  ShowAttribute ChallengeA       = "challenge"
  ShowAttribute CharsetA         = "charset"
  ShowAttribute CheckedA         = "checked"
  ShowAttribute CiteA            = "cite"
  ShowAttribute ClassA           = "class"
  ShowAttribute CodeA            = "code"
  ShowAttribute CodebaseA        = "codebase"
  ShowAttribute ColorA           = "color"
  ShowAttribute ColsA            = "cols"
  ShowAttribute ColspanA         = "colspan"
  ShowAttribute ContentA         = "content"
  ShowAttribute ContenteditableA = "contenteditable"
  ShowAttribute ContextmenuA     = "contextmenu"
  ShowAttribute ControlsA        = "controls"
  ShowAttribute CoordsA          = "coords"
  ShowAttribute CrossoriginA     = "crossorigin"
  ShowAttribute DataA            = "data"
  ShowAttribute Data'A           = "data'"
  ShowAttribute DatetimeA        = "datetime"
  ShowAttribute DefaultA         = "default"
  ShowAttribute DeferA           = "defer"
  ShowAttribute DirA             = "dir"
  ShowAttribute DirnameA         = "dirname"
  ShowAttribute DisabledA        = "disabled"
  ShowAttribute DownloadA        = "download"
  ShowAttribute DraggableA       = "draggable"
  ShowAttribute DropzoneA        = "dropzone"
  ShowAttribute EnctypeA         = "enctype"
  ShowAttribute ForA             = "for"
  ShowAttribute FormA            = "form"
  ShowAttribute FormactionA      = "formaction"
  ShowAttribute HeadersA         = "headers"
  ShowAttribute HeightA          = "height"
  ShowAttribute HiddenA          = "hidden"
  ShowAttribute HighA            = "high"
  ShowAttribute HrefA            = "href"
  ShowAttribute HreflangA        = "hreflang"
  ShowAttribute HttpEquivA       = "httpequiv"
  ShowAttribute IconA            = "icon"
  ShowAttribute IdA              = "id"
  ShowAttribute IntegrityA       = "integrity"
  ShowAttribute IsmapA           = "ismap"
  ShowAttribute ItempropA        = "itemprop"
  ShowAttribute KeytypeA         = "keytype"
  ShowAttribute KindA            = "kind"
  ShowAttribute LabelA           = "label"
  ShowAttribute LangA            = "lang"
  ShowAttribute LanguageA        = "language"
  ShowAttribute ListA            = "list"
  ShowAttribute LoopA            = "loop"
  ShowAttribute LowA             = "low"
  ShowAttribute ManifestA        = "manifest"
  ShowAttribute MaxA             = "max"
  ShowAttribute MaxlengthA       = "maxlength"
  ShowAttribute MinlengthA       = "minlength"
  ShowAttribute MediaA           = "media"
  ShowAttribute MethodA          = "method"
  ShowAttribute MinA             = "min"
  ShowAttribute MultipleA        = "multiple"
  ShowAttribute MutedA           = "muted"
  ShowAttribute NameA            = "name"
  ShowAttribute NovalidateA      = "novalidate"
  ShowAttribute OpenA            = "open"
  ShowAttribute OptimumA         = "optimum"
  ShowAttribute PatternA         = "pattern"
  ShowAttribute PingA            = "ping"
  ShowAttribute PlaceholderA     = "placeholder"
  ShowAttribute PosterA          = "poster"
  ShowAttribute PreloadA         = "preload"
  ShowAttribute RadiogroupA      = "radiogroup"
  ShowAttribute ReadonlyA        = "readonly"
  ShowAttribute RelA             = "rel"
  ShowAttribute RequiredA        = "required"
  ShowAttribute ReversedA        = "reversed"
  ShowAttribute RowsA            = "rows"
  ShowAttribute RowspanA         = "rowspan"
  ShowAttribute SandboxA         = "sandbox"
  ShowAttribute ScopeA           = "scope"
  ShowAttribute ScopedA          = "scoped"
  ShowAttribute SeamlessA        = "seamless"
  ShowAttribute SelectedA        = "selected"
  ShowAttribute ShapeA           = "shape"
  ShowAttribute SizeA            = "size"
  ShowAttribute SizesA           = "sizes"
  ShowAttribute SlotA            = "slot"
  ShowAttribute SpanA            = "span"
  ShowAttribute SpellcheckA      = "spellcheck"
  ShowAttribute SrcA             = "src"
  ShowAttribute SrcdocA          = "srcdoc"
  ShowAttribute SrclangA         = "srclang"
  ShowAttribute SrcsetA          = "srcset"
  ShowAttribute StartA           = "start"
  ShowAttribute StepA            = "step"
  ShowAttribute StyleA           = "style"
  ShowAttribute SummaryA         = "summary"
  ShowAttribute TabindexA        = "tabindex"
  ShowAttribute TargetA          = "target"
  ShowAttribute TitleA           = "title"
  ShowAttribute TypeA            = "type"
  ShowAttribute UsemapA          = "usemap"
  ShowAttribute ValueA           = "value"
  ShowAttribute WidthA           = "width"
  ShowAttribute WrapA            = "wrap"

type family OpenTag e where
  OpenTag e = AppendSymbol (AppendSymbol "<" (ShowElement e)) ">"

type family CloseTag e where
  CloseTag e = AppendSymbol (AppendSymbol "</" (ShowElement e)) ">"

type family CountContent c where
  CountContent (a # b)       = CountContent a + CountContent b
  CountContent (_ > b)       = CountContent b
  CountContent ((_ :@: b) c) = CountContent b + CountContent c
  CountContent ()            = 0
  CountContent _             = 1

-- | We need efficient cons, snoc and append.  This API has cons(O1)
-- and snoc(O1) but append(On).  Optimal would be a real 2-3
-- FingerTree.
data FingerTree = FingerTree [Symbol] Symbol

type family (<|) (s :: Symbol) (t :: FingerTree) :: FingerTree where
  (<|) l ('FingerTree (s ': ss) r) = 'FingerTree (AppendSymbol l s ': ss) r
  (<|) l ('FingerTree '[] r) = 'FingerTree '[] (AppendSymbol l r)

type family (|>) (t :: FingerTree) (s :: Symbol) :: FingerTree where
  (|>) ('FingerTree ss r) rr = 'FingerTree ss (AppendSymbol r rr)

type family (><) (t1 :: FingerTree) (t2 :: FingerTree) :: FingerTree where
  (><) ('FingerTree ss r) ('FingerTree (s ': ss2) r2) = 'FingerTree (Append ss (AppendSymbol r s ': ss2)) r2
  (><) ('FingerTree ss r) ('FingerTree '[] r2) = 'FingerTree ss (AppendSymbol r r2)

type family ToList (t :: FingerTree) where
  ToList ('FingerTree ss r) = Append ss '[r]

-- | Flatten a html tree of elements into a type list of tags.
type family ToTypeList a :: FingerTree where
  ToTypeList (a # ())       = ToTypeList a
  ToTypeList (() # b)       = ToTypeList b
  ToTypeList (a # b)        = ToTypeList a >< ToTypeList b
  ToTypeList (a > ())       = 'FingerTree '[] (If (HasContent (GetInfo a)) (AppendSymbol (OpenTag a) (CloseTag a)) (OpenTag a))
  ToTypeList ((a :@: b) ()) = AppendSymbol "<" (ShowElement a) <| ToTypeList b |> If (HasContent (GetInfo a)) (AppendSymbol ">" (CloseTag a)) ">"
  ToTypeList (a > b)        = OpenTag a <| ToTypeList b |> CloseTag a
  ToTypeList ((a :@: b) c)  = (AppendSymbol "<" (ShowElement a) <| ToTypeList b) >< (">" <| ToTypeList c |> CloseTag a)
  ToTypeList (a := b)       = 'FingerTree '[AppendSymbol (AppendSymbol " " (ShowAttribute a)) "=\""] "\""
  ToTypeList (Proxy x)      = 'FingerTree '[] x
  ToTypeList x              = 'FingerTree '[""] ""

-- | Append two type lists.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Append xs ys :: [Symbol] where

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

-- | Check whether an element may have content.
type family HasContent a where
  HasContent (ElementInfo _ NoContent) = False
  HasContent _                         = True

-- | Type level drop.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Drop n xs :: [Symbol] where
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
type family Take n xs :: [Symbol] where
  Take 0 _ = '[]
  Take 1 (x1 ': _) = '[x1]
  Take 2 (x1 ': x2 ': _) = '[x1, x2]
  Take 3 (x1 ': x2 ': x3 ': _) = '[x1, x2, x3]
  Take 4 (x1 ': x2 ': x3 ': x4 ': _) = '[x1, x2, x3, x4]
  Take n (x1 ': x2 ': x3 ': x4 ': x5 ': xs) = x1 ': x2 ': x3 ': x4 ': x5 ': Take (n-5) xs

-- | Last for type level lists.
--
-- Note that this definition is that ugly to reduce compiletimes.
-- Please check whether the context reduction stack or compiletimes of
-- a big html page get bigger if you try to refactor.
type family Last (xs :: [Symbol]) where
  Last (_ ': _ ': _ ': _ ': _ ': _ ': _ ': _ ': x ': xs) = Last (x ': xs)
  Last (_ ': _ ': _ ': _ ': x ': xs) = Last (x ': xs)
  Last (_ ': _ ': x ': xs) = Last (x ': xs)
  Last (_ ': x ': xs) = Last (x ': xs)
  Last (x ': xs) = x

-- | Type of type level information about tags.
data ElementInfo
  (contentCategories :: [ContentCategory])
  (permittedContent  :: ContentCategory)

type family TestPaternity a b c :: Bool where
  TestPaternity a (ElementInfo _ ps) (ElementInfo cs _) = CheckContentCategory ps (a ': cs)

type family CheckContentCategory (a :: ContentCategory) (b :: [ContentCategory]) :: Bool where
  CheckContentCategory (a :|: b) c = CheckContentCategory a c || CheckContentCategory b c
  CheckContentCategory (a :&: b) c = CheckContentCategory a c && CheckContentCategory b c
  CheckContentCategory (NOT a) c   = Not (CheckContentCategory a c)
  CheckContentCategory a c         = Elem a c

-- | Check whether a given element may contain a string.
type family CheckString (a :: Element) b where
  CheckString a b = If (TestPaternity OnlyText (GetInfo a) (ElementInfo '[FlowContent, PhrasingContent] NoContent))
                       (() :: Constraint)
                       (TypeError (ShowType a :<>: Text " can't contain a " :<>: ShowType b))

-- | Content categories according to the html spec.
data ContentCategory
  = MetadataContent
  | FlowContent
  | SectioningContent
  | HeadingContent
  | PhrasingContent
  | EmbeddedContent
  | InteractiveContent
  | FormAssociatedContent
  | TransparentContent
  | PalpableContent
  | SectioningRoot
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

newtype Tagged (proxies :: [Symbol]) target = Tagged target

type Symbols a = ToList (ToTypeList a)

-- | Get type list of valid elements for a given attribute.  An empty list signifies global attribute.
type family GetAttributeInfo a where
  GetAttributeInfo AcceptA          = '[Form, Input]
  GetAttributeInfo AcceptCharsetA   = '[Form]
  GetAttributeInfo AccesskeyA       = '[]
  GetAttributeInfo ActionA          = '[Form]
  GetAttributeInfo AlignA           = '[Applet, Caption, Col, Colgroup, Hr, Iframe, Img, Table, Tbody, Td, Tfoot, Th, Thead, Tr]
  GetAttributeInfo AltA             = '[Applet, Area, Img, Input]
  GetAttributeInfo AsyncA           = '[Script]
  GetAttributeInfo AutocompleteA    = '[Form, Input]
  GetAttributeInfo AutofocusA       = '[Button, Input, Keygen, Select, Textarea]
  GetAttributeInfo AutoplayA        = '[Audio, Video]
  GetAttributeInfo AutosaveA        = '[Input]
  GetAttributeInfo BgcolorA         = '[Body, Col, Colgroup, Marquee, Table, Tbody, Tfoot, Td, Th, Tr]
  GetAttributeInfo BorderA          = '[Img, Object, Table]
  GetAttributeInfo BufferedA        = '[Audio, Video]
  GetAttributeInfo ChallengeA       = '[Keygen]
  GetAttributeInfo CharsetA         = '[Meta, Script]
  GetAttributeInfo CheckedA         = '[Command, Input]
  GetAttributeInfo CiteA            = '[Blockquote, Del, Ins, Q]
  GetAttributeInfo ClassA           = '[]
  GetAttributeInfo CodeA            = '[Applet]
  GetAttributeInfo CodebaseA        = '[Applet]
  GetAttributeInfo ColorA           = '[Basefont, Font, Hr]
  GetAttributeInfo ColsA            = '[Textarea]
  GetAttributeInfo ColspanA         = '[Td, Th]
  GetAttributeInfo ContentA         = '[Meta]
  GetAttributeInfo ContenteditableA = '[]
  GetAttributeInfo ContextmenuA     = '[]
  GetAttributeInfo ControlsA        = '[Audio, Video]
  GetAttributeInfo CoordsA          = '[Area]
  GetAttributeInfo CrossoriginA     = '[Audio, Img, Link, Script, Video]
  GetAttributeInfo DataA            = '[Object]
  GetAttributeInfo DatetimeA        = '[Del, Ins, Time]
  GetAttributeInfo DefaultA         = '[Track]
  GetAttributeInfo DeferA           = '[Script]
  GetAttributeInfo DirA             = '[]
  GetAttributeInfo DirnameA         = '[Input, Textarea]
  GetAttributeInfo DisabledA        = '[Button, Command, Fieldset, Input, Keygen, Optgroup, Option, Select, Textarea]
  GetAttributeInfo DownloadA        = '[A, Area]
  GetAttributeInfo DraggableA       = '[]
  GetAttributeInfo DropzoneA        = '[]
  GetAttributeInfo EnctypeA         = '[Form]
  GetAttributeInfo ForA             = '[Label, Output]
  GetAttributeInfo FormA            = '[Button, Fieldset, Input, Keygen, Label, Meter, Object, Output, Progress, Select, Textarea]
  GetAttributeInfo FormactionA      = '[Input, Button]
  GetAttributeInfo HeadersA         = '[Td, Th]
  GetAttributeInfo HeightA          = '[Canvas, Embed, Iframe, Img, Input, Object, Video]
  GetAttributeInfo HiddenA          = '[]
  GetAttributeInfo HighA            = '[Meter]
  GetAttributeInfo HrefA            = '[A, Area, Base, Link]
  GetAttributeInfo HreflangA        = '[A, Area, Link]
  GetAttributeInfo HttpEquivA       = '[Meta]
  GetAttributeInfo IconA            = '[Command]
  GetAttributeInfo IdA              = '[]
  GetAttributeInfo IntegrityA       = '[Link, Script]
  GetAttributeInfo IsmapA           = '[Img]
  GetAttributeInfo ItempropA        = '[]
  GetAttributeInfo KeytypeA         = '[Keygen]
  GetAttributeInfo KindA            = '[Track]
  GetAttributeInfo LabelA           = '[Track]
  GetAttributeInfo LangA            = '[]
  GetAttributeInfo LanguageA        = '[Script]
  GetAttributeInfo ListA            = '[Input]
  GetAttributeInfo LoopA            = '[Audio, Bgsound, Marquee, Video]
  GetAttributeInfo LowA             = '[Meter]
  GetAttributeInfo ManifestA        = '[Html]
  GetAttributeInfo MaxA             = '[Input, Meter, Progress]
  GetAttributeInfo MaxlengthA       = '[Input, Textarea]
  GetAttributeInfo MinlengthA       = '[Input, Textarea]
  GetAttributeInfo MediaA           = '[A, Area, Link, Source, Style]
  GetAttributeInfo MethodA          = '[Form]
  GetAttributeInfo MinA             = '[Input, Meter]
  GetAttributeInfo MultipleA        = '[Input, Select]
  GetAttributeInfo MutedA           = '[Video]
  GetAttributeInfo NameA            = '[Button, Form, Fieldset, Iframe, Input, Keygen, Object, Output, Select, Textarea, Map, Meta, Param]
  GetAttributeInfo NovalidateA      = '[Form]
  GetAttributeInfo OpenA            = '[Details]
  GetAttributeInfo OptimumA         = '[Meter]
  GetAttributeInfo PatternA         = '[Input]
  GetAttributeInfo PingA            = '[A, Area]
  GetAttributeInfo PlaceholderA     = '[Input, Textarea]
  GetAttributeInfo PosterA          = '[Video]
  GetAttributeInfo PreloadA         = '[Audio, Video]
  GetAttributeInfo RadiogroupA      = '[Command]
  GetAttributeInfo ReadonlyA        = '[Input, Textarea]
  GetAttributeInfo RelA             = '[A, Area, Link]
  GetAttributeInfo RequiredA        = '[Input, Select, Textarea]
  GetAttributeInfo ReversedA        = '[Ol]
  GetAttributeInfo RowsA            = '[Textarea]
  GetAttributeInfo RowspanA         = '[Td, Th]
  GetAttributeInfo SandboxA         = '[Iframe]
  GetAttributeInfo ScopeA           = '[Th]
  GetAttributeInfo ScopedA          = '[Style]
  GetAttributeInfo SeamlessA        = '[Iframe]
  GetAttributeInfo SelectedA        = '[Option]
  GetAttributeInfo ShapeA           = '[A, Area]
  GetAttributeInfo SizeA            = '[Input, Select]
  GetAttributeInfo SizesA           = '[Link, Img, Source]
  GetAttributeInfo SlotA            = '[]
  GetAttributeInfo SpanA            = '[Col, Colgroup]
  GetAttributeInfo SpellcheckA      = '[]
  GetAttributeInfo SrcA             = '[Audio, Embed, Iframe, Img, Input, Script, Source, Track, Video]
  GetAttributeInfo SrcdocA          = '[Iframe]
  GetAttributeInfo SrclangA         = '[Track]
  GetAttributeInfo SrcsetA          = '[Img]
  GetAttributeInfo StartA           = '[Ol]
  GetAttributeInfo StepA            = '[Input]
  GetAttributeInfo StyleA           = '[]
  GetAttributeInfo SummaryA         = '[Table]
  GetAttributeInfo TabindexA        = '[]
  GetAttributeInfo TargetA          = '[A, Area, Base, Form]
  GetAttributeInfo TitleA           = '[]
  GetAttributeInfo TypeA            = '[Button, Input, Command, Embed, Object, Script, Source, Style, Menu]
  GetAttributeInfo UsemapA          = '[Img, Input, Object]
  GetAttributeInfo ValueA           = '[Button, Option, Input, Li, Meter, Progress, Param]
  GetAttributeInfo WidthA           = '[Canvas, Embed, Iframe, Img, Input, Object, Video]
  GetAttributeInfo WrapA            = '[Textarea]

-- | Retrieve type level meta data about elements.
type family GetInfo a where

  GetInfo DOCTYPE = ElementInfo
    '[]
    NoContent

  GetInfo A = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, PalpableContent ]
    (TransparentContent :|: FlowContent :&: NOT InteractiveContent :|: PhrasingContent)

  GetInfo Abbr = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Address = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (HeadingContent :|: SectioningContent :|: SingleElement Address :|: SingleElement Header :|: SingleElement Footer))

  GetInfo Area = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent

  GetInfo Article = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent

  GetInfo Aside = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent

  GetInfo Audio = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (SingleElement Source :|: SingleElement Track :|: TransparentContent :&: NOT (SingleElement Audio :|: SingleElement Video))

  GetInfo B = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Base = ElementInfo
    '[ MetadataContent ]
    NoContent

  GetInfo Bdi = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Bdo = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Blockquote = ElementInfo
    [ FlowContent, SectioningRoot, PalpableContent ]
    FlowContent

  GetInfo Body = ElementInfo
    '[ SectioningRoot ]
    FlowContent

  GetInfo Br = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent

  GetInfo Button = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, PalpableContent ]
    PhrasingContent

  GetInfo Canvas = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent ]
    (TransparentContent :&: NOT InteractiveContent :|: SingleElement A :|: SingleElement Button :|: SingleElement Input)

  GetInfo Caption = ElementInfo
    '[]
    FlowContent

  GetInfo Cite = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Code = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Col = ElementInfo
    '[]
    NoContent

  GetInfo Colgroup = ElementInfo
    '[]
    (SingleElement Col)

  GetInfo Data = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Datalist = ElementInfo
    [ FlowContent, PhrasingContent ]
    (PhrasingContent :|: SingleElement Option)

  GetInfo Dd = ElementInfo
    '[]
    FlowContent

  GetInfo Del = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent

  GetInfo Details = ElementInfo
    [ FlowContent, SectioningRoot, InteractiveContent, PalpableContent ]
    ( SingleElement Summary :|: FlowContent)

  GetInfo Dfn = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Dfn))

  GetInfo Dialog = ElementInfo
    [ FlowContent, SectioningRoot ]
    FlowContent

  GetInfo Div = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :|: SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template)

  GetInfo Dl = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template :|: SingleElement Div)

  GetInfo Dt = ElementInfo
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))

  GetInfo Em = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Embed = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    NoContent

  GetInfo Fieldset = ElementInfo
    [ FlowContent, SectioningRoot, FormAssociatedContent, PalpableContent ]
    (SingleElement Legend :|: FlowContent)

  GetInfo Figcaption = ElementInfo
    '[]
    FlowContent

  GetInfo Figure = ElementInfo
    [ FlowContent, SectioningRoot, PalpableContent ]
    (SingleElement Figcaption :|: FlowContent)

  GetInfo Footer = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Footer :|: SingleElement Header))

  GetInfo Form = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Form))

  GetInfo H1 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo H2 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo H3 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo H4 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo H5 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo H6 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent

  GetInfo Head = ElementInfo
    '[]
    (MetadataContent :|: SingleElement Title)

  GetInfo Header = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer))

  GetInfo Hgroup = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    (SingleElement H1 :|: SingleElement H2 :|: SingleElement H3 :|: SingleElement H4 :|: SingleElement H5 :|: SingleElement H6)

  GetInfo Hr = ElementInfo
    '[ FlowContent ]
    NoContent

  GetInfo Html = ElementInfo
    '[]
    (SingleElement Head :|: SingleElement Body)

  GetInfo I = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Iframe = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    NoContent

  GetInfo Img = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent, InteractiveContent ]
    NoContent

  GetInfo Ins = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent

  GetInfo Kbd = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Label = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Label))

  GetInfo Legend = ElementInfo
    '[]
    PhrasingContent

  GetInfo Li = ElementInfo
    '[]
    FlowContent

  GetInfo Link = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    NoContent

  GetInfo Main = ElementInfo
    [ FlowContent, PalpableContent ]
    FlowContent

  GetInfo Map = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    TransparentContent

  GetInfo Mark = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Menu = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :|: SingleElement Li :|: SingleElement Script :|: SingleElement Template :|: SingleElement Menu :|: SingleElement Menuitem :|: SingleElement Hr)

  GetInfo Menuitem = ElementInfo
    '[]
    NoContent

  GetInfo Meta = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    NoContent

  GetInfo Meter = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Meter))

  GetInfo Nav = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent

  GetInfo Noscript = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    (SingleElement Link :|: SingleElement Style :|: SingleElement Meta :|: TransparentContent :&: NOT (SingleElement Noscript) :|: FlowContent :|: PhrasingContent)

  GetInfo Object = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent, InteractiveContent, FormAssociatedContent ]
    (SingleElement Param :|: TransparentContent)

  GetInfo Ol = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Li)

  GetInfo Optgroup = ElementInfo
    '[]
    (SingleElement Option)

  GetInfo Option = ElementInfo
    '[]
    OnlyText

  GetInfo Output = ElementInfo
    [ FlowContent, PhrasingContent, FormAssociatedContent, PalpableContent ]
    PhrasingContent

  GetInfo P = ElementInfo
    [ FlowContent, PalpableContent ]
    PhrasingContent

  GetInfo Param = ElementInfo
    '[]
    NoContent

  GetInfo Picture = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent ]
    (SingleElement Source :|: SingleElement Img)

  GetInfo Pre = ElementInfo
    [ FlowContent, PalpableContent ]
    PhrasingContent

  GetInfo Progress = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Progress))

  GetInfo Q = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Rp = ElementInfo
    '[]
    OnlyText

  GetInfo Rt = ElementInfo
    '[]
    PhrasingContent

  GetInfo Rtc = ElementInfo
    '[]
    (PhrasingContent :|: SingleElement Rt)

  GetInfo Ruby = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo S = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent

  GetInfo Samp = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Script = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    OnlyText

  GetInfo Section = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent

  GetInfo Select = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent ]
    (SingleElement Option :|: SingleElement Optgroup)

  GetInfo Slot = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent

  GetInfo Small = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent

  GetInfo Source = ElementInfo
    '[]
    NoContent

  GetInfo Span = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent

  GetInfo Strong = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Style = ElementInfo
    [ MetadataContent, FlowContent ]
    OnlyText

  GetInfo Sub = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent

  GetInfo Summary = ElementInfo
    '[]
    (PhrasingContent :|: HeadingContent)

  GetInfo Sup = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent

  GetInfo Table = ElementInfo
    '[FlowContent]
    (SingleElement Caption :|: SingleElement Colgroup :|: SingleElement Thead :|: SingleElement Tbody :|: SingleElement Tr :|: SingleElement Tfoot)

  GetInfo Tbody = ElementInfo
    '[]
    (SingleElement Tr)

  GetInfo Td = ElementInfo
    '[]
    FlowContent

  GetInfo Template = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    (MetadataContent :|: FlowContent)

  GetInfo Textarea = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent ]
    OnlyText

  GetInfo Tfoot = ElementInfo
    '[]
    (SingleElement Tr)

  GetInfo Th = ElementInfo
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))

  GetInfo Thead = ElementInfo
    '[]
    (SingleElement Tr)

  GetInfo Time = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Title = ElementInfo
    '[ MetadataContent ]
    OnlyText

  GetInfo Tr = ElementInfo
    '[]
    (SingleElement Td :|: SingleElement Th)

  GetInfo Track = ElementInfo
    '[]
    NoContent

  GetInfo U = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Ul = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Li)

  GetInfo Var = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent

  GetInfo Video = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (SingleElement Track :|: TransparentContent :&: NOT (SingleElement Audio :|: SingleElement Video) :|: SingleElement Source)

  GetInfo Wbr = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent

  GetInfo _ = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (FlowContent :|: PhrasingContent :|: EmbeddedContent :|: InteractiveContent :|: PalpableContent)
