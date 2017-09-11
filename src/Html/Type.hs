{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}

module Html.Type where

import qualified Data.ByteString.Builder as B

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Bool
import qualified Data.Semigroup as S
import qualified Data.Monoid as M

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

-- | Check whether `b` is a valid child of `a`.  You'll propably never
-- need to call this directly.  Through a GADT, it is enforced that
-- every child is lawful.
type family (a :: Element) ?> b :: Constraint where
  a ?> (b # c)    = (a ?> b, a ?> c)
  a ?> (b > _)    = MaybeTypeError a b (TestPaternity (SingleElement b) (GetInfo a) (GetInfo b))
  a ?> (b :> _)   = MaybeTypeError a b (TestPaternity (SingleElement b) (GetInfo a) (GetInfo b))
  a ?> Maybe b    = a ?> b
  a ?> Either b c = (a ?> b, a ?> c)
  a ?> f (b > c)  = a ?> (b > c)
  a ?> f (b :> c) = a ?> (b > c)
  a ?> f (b # c)  = a ?> (b # c)
  a ?> ()         = ()
  a ?> (b -> c)   = TypeError (Text "Html elements can't contain functions")
  a ?> b          = CheckString a

-- | Combine two elements sequentially.
--
-- >>> render (i_ () # div_ ()) :: String
-- "<i></i><div></div>"
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
data (:>) (a :: Element) b where
  WithAttributes :: (a ?> b) => Attribute -> b -> a :> b
infixr 8 :>

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw a

  -------------------
  -- internal code --
  -------------------

newtype Attribute = Attribute B.Builder deriving (M.Monoid, S.Semigroup)

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

type family OpenTag e where
  OpenTag e = AppendSymbol (AppendSymbol "<" (ShowElement e)) ">"

type family CloseTag e where
  CloseTag e = AppendSymbol (AppendSymbol "</" (ShowElement e)) ">"

type family CountContent c where
  CountContent (a # b)  = CountContent a + CountContent b
  CountContent (a > b)  = CountContent b
  CountContent (a :> b) = 1 + CountContent b
  CountContent ()       = 0
  CountContent _        = 1

-- | Flatten a html tree of elements into a type list of tags.
type family ToTypeList a where
  ToTypeList (() # b)  = ToTypeList b
  ToTypeList (a # ())  = ToTypeList a
  ToTypeList (a # b)   = Append (ToTypeList a) (ToTypeList b)
  ToTypeList (a > ())  = If (HasContent (GetInfo a)) '[OpenTag a, CloseTag a] '[OpenTag a]
  ToTypeList (a :> ()) = If (HasContent (GetInfo a)) '[AppendSymbol "<" (ShowElement a), "", ">", CloseTag a] '[AppendSymbol "<" (ShowElement a), "", ">"]
  ToTypeList (a > b)   = Append (OpenTag a ': ToTypeList b) '[CloseTag a]
  ToTypeList (a :> b)  = Append (AppendSymbol "<" (ShowElement a) ': "" ': ">" ': ToTypeList b) '[CloseTag a]
  ToTypeList (Proxy x) = '[x]
  ToTypeList x         = '[""]

-- | Append two type lists.
type family Append xs ys :: [Symbol] where
  Append xs '[]       = xs
  Append '[] ys       = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Check whether an element may have content.
type family HasContent a where
  HasContent (ElementInfo _ NoContent) = False
  HasContent _                         = True

-- | Fuse neighbouring non empty type level strings.
type family Fuse a where
  Fuse ("" ': xs)      = "" ': Fuse xs
  Fuse '[a, ""]        = '[a, ""]
  Fuse (a ': "" ': xs) = a ': Fuse xs
  Fuse (a ': b ': xs)  = Fuse (AppendSymbol a b ': xs)
  Fuse (a ': xs)       = '[a]
  Fuse '[]             = '[]

type family Drop n xs :: [Symbol] where
  Drop 0 xs = xs
  Drop n (_ ': xs) = Drop (n-1) xs

type family Take n xs :: [Symbol] where
  Take 0 _ = '[]
  Take n (x ': xs) = x ': Take (n-1) xs

-- | Last for type level lists.
type family Last (xs :: [Symbol]) where
  Last (_ ': x ': xs) = Last (x ': xs)
  Last (x ': xs) = x
  Last _         = ""

-- | Head for type level lists.
type family Head' a :: Symbol where
  Head' (a ': _) = a

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
type family CheckString (a :: Element) where
  CheckString a = If (TestPaternity OnlyText (GetInfo a) (ElementInfo '[FlowContent, PhrasingContent] NoContent))
                     (() :: Constraint)
                     (TypeError (ShowType a :<>: Text " can't contain a string"))

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

type family Elem (a :: ContentCategory) (xs :: [ContentCategory]) where
  Elem a (a : xs) = True
  Elem a (_ : xs) = Elem a xs
  Elem a '[]      = False

newtype Tagged (proxies :: [Symbol]) target = Tagged target

type Symbols a = Fuse (ToTypeList a)

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
    (MetadataContent :|: FlowContent) -- complicated

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
