{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}

module Html.Type where

import qualified Data.Text.Lazy as T

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

-- | Check whether `b` is a valid child of `a`.  You'll propably never
-- need to call this directly.  Through a GADT, it is enforced that
-- every child is lawful.
--
-- The only way to circumvent this would be to use 'undefined' or
-- 'error' in combination with only type level values.
--
-- >>> undefined :: 'Div > ('Html > ())
-- <div><html></html></div>
--
-- >>> undefined :: 'Div > ('Html > Proxy "a")
-- <div><html>a</html></div>
--
-- >>> undefined :: 'Div > ('Html > String)
-- <div><html>*** Exception: Prelude.undefined
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
  a ?> (b -> c)   = (TypeError (Text "Html elements can't contain functions"))
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
-- >>> WithAttributes [A.class_ "bar"] "a" :: 'Div :> String
-- <div class="bar">a</div>
data (:>) (a :: Element) b where
  WithAttributes :: (a ?> b) => [Attribute] -> b -> a :> b
infixr 8 :>

{-# INLINE addAttributes #-}
addAttributes :: (a ?> b) => [Attribute] -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes xs b

-- | Wrapper for types which won't be escaped.
newtype Raw a = Raw a

newtype Converted a = Converted {unConv :: a}

  -------------------
  -- internal code --
  -------------------

newtype Attribute = Attribute T.Text

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
  ToTypeList (Next val nex) = Next (ToTypeList val) (ToTypeList nex)
  ToTypeList (a # b)        = Append (ToTypeList a) (ToTypeList b)
  ToTypeList (a > ())       = If (HasContent (GetInfo a)) (Open a, Close a) (Open a)
  ToTypeList (a :> ())      = If (HasContent (GetInfo a)) (OpenAttr a, (Attribute, (EndOfOpen, Close a))) (OpenAttr a, (Attribute, EndOfOpen))
  ToTypeList (a > b)        = Append (Open a, ToTypeList b) (Close a)
  ToTypeList (a :> b)       = Append (OpenAttr a, (Attribute, (EndOfOpen, ToTypeList b))) (Close a)
  ToTypeList x              = x

-- | Append two type lists.
type family Append a b where
  Append (a, b) c = (a, Append b c)
  Append a      b = (a, b)

-- | Check whether an element may have content.
type family HasContent a where
  HasContent (ElementInfo _ NoContent _) = False
  HasContent _                           = True

-- | Remove omittable and empty tags from a type list of tags.
type family PruneTags a where
  PruneTags ((), a)      = PruneTags a
  PruneTags (a , ())     = PruneTags a
  PruneTags (Close a, b) = IsOmittable (GetInfo a) (Close a, b)
  PruneTags (Next a b)   = IsOmittableL (Head' (PruneTags a)) (PruneTags a) (Last (PruneTags a)) b
  PruneTags (a, b)       = (a, PruneTags b)
  PruneTags a            = a

data Next v nex

type family IsOmittableL head val last next where
  IsOmittableL (OpenAttr head) val (Close last) (Close next) = IsOmittable2 (Open head) val (GetInfo last) (Close next)
  IsOmittableL (Open head)     val (Close last) (Close next) = IsOmittable2 (Open head) val (GetInfo last) (Close next)

-- Base case
  IsOmittableL _head val _last next = val

type family IsOmittable2 head list last next where
  IsOmittable2
    (Open head)
    val
    (ElementInfo _ _ (LastChildOrFollowedBy (head ': _))) -- last
    (Close next)
    = Init val

  IsOmittable2
    (Open head)
    val
    (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) -- last
    (Close next)
    = IsOmittable2 (Open head) val (ElementInfo a b (LastChildOrFollowedBy xs)) (Close next)

-- Base case
  IsOmittable2
    _head
    val
    _last
    next
    = val

-- | Checks whether a tag is omittable.  Sadly, returning a kind Bool
-- would make the compiler loop, so we inline the if into the type
-- function.
type family IsOmittable a b where
  IsOmittable (ElementInfo _ _ RightOmission) (_,c)                                   = PruneTags c
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy _)) (_,(Close b, c))            = PruneTags (Close b, c)
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy _)) (_,Close b)                 = Close b
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy '[])) (b,c)                     = (b, PruneTags c)
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy (x ': _))) (c, (Open x, d))     = PruneTags (Open x, d)
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy (x ': _))) (c, (OpenAttr x, d)) = PruneTags (OpenAttr x, d)
  IsOmittable (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) c                   = IsOmittable (ElementInfo a b (LastChildOrFollowedBy xs)) c
  IsOmittable _ (c,d)                                                                 = (c, PruneTags d)

-- | Convert tags to type level strings.
type family RenderTags a where
  RenderTags (a, b)       = (RenderTags a, RenderTags b)
  RenderTags (Open a)     = Proxy (OpenTag a)
  RenderTags (OpenAttr a) = Proxy (AppendSymbol "<" (ShowElement a))
  RenderTags (Close a)    = Proxy (CloseTag a)
  RenderTags EndOfOpen    = Proxy ">"
  RenderTags a            = a

-- | Fuse neighbouring type level strings.
type family Fuse a where
  Fuse (Proxy (a :: Symbol), (Proxy (b :: Symbol), c)) = Fuse (Proxy (AppendSymbol a b), c)
  Fuse (Proxy (a :: Symbol), Proxy (b :: Symbol))      = '[AppendSymbol a b]
  Fuse (Proxy (a :: Symbol), (b,c))                    =  a ': Fuse c
  Fuse (Proxy (a :: Symbol), b)                        = a ': Fuse b
  Fuse (Proxy (a :: Symbol))                           = '[a]
  Fuse (a, b)                                          = "" ': Fuse b
  Fuse a                                               = '[""]

type family Drop n xs :: [Symbol] where
  Drop 0 xs = xs
  Drop 1 (_ ': xs) = xs
  Drop 2 (_ ': _ ': xs) = xs
  Drop 3 (_ ': _ ': _ ': xs) = xs
  Drop 4 (_ ': _ ': _ ': _ ': xs) = xs
  Drop n (_ ': _ ': _ ': _ ': _ ': xs) = Drop (n-5) xs

type family Take n xs :: [Symbol] where
  Take 0 _ = '[]
  Take 1 (x1 ': _) = '[x1]
  Take 2 (x1 ': x2 ': _) = [x1,x2]
  Take 3 (x1 ': x2 ': x3 ': _) = [x1,x2,x3]
  Take 4 (x1 ': x2 ': x3 ': x4 ': _) = [x1,x2,x3,x4]
  Take n (x1 ': x2 ': x3 ': x4 ': x5 ': xs) = x1 ': x2 ': x3 ': x4 ': x5 ': Take (n-5) xs

-- | Init for type level lists.
type family Init xs where
  Init (a, (b, (c, d))) = (a, (b, Init (c, d)))
  Init (a, (b,c)) = (a, b)
  Init (a, b)     = a
  Init a          = a

-- | Last for type level lists.
type family Last a where
  Last (a, (b, (c, d))) = Last d
  Last (a, (b, c)) = c
  Last (a, b) = b
  Last a = a

-- | Last for type level lists.
type family Last' (xs :: [Symbol]) where
  Last' (_ ': _ ': _ ': _ ': x ': xs) = Last' (x ': xs)
  Last' (_ ': _ ': _ ': x ': xs) = x
  Last' (_ ': _ ': x ': xs) = x
  Last' (_ ': x ': xs) = x
  Last' (x ': xs) = x
  Last' _         = ""

-- | Head for type level lists.
type family Head' a where
  Head' (a, as) = a
  Head' a       = a

type family HeadL a :: Symbol where
  HeadL (a ': _) = a

-- | Utility types.
data EndOfOpen
data Open (a :: Element)
data OpenAttr (a :: Element)
data Close (a :: Element)

-- | Type of type level information about tags.
data ElementInfo
  (contentCategories :: [ContentCategory])
  (permittedContent  :: ContentCategory)
  (tagOmission       :: TagOmission)

-- | Kind describing whether it is valid to delete a closing tag.
data TagOmission
  = NoOmission
  | RightOmission
  | LastChildOrFollowedBy [Element]

type family TestPaternity a b c :: Bool where
  TestPaternity a (ElementInfo _ ps _) (ElementInfo cs _ _) = CheckContentCategory ps (a ': cs)

type family CheckContentCategory (a :: ContentCategory) (b :: [ContentCategory]) :: Bool where
  CheckContentCategory (a :|: b) c = CheckContentCategory a c || CheckContentCategory b c
  CheckContentCategory (a :&: b) c = CheckContentCategory a c && CheckContentCategory b c
  CheckContentCategory (NOT a) c   = Not (CheckContentCategory a c)
  CheckContentCategory a c         = Elem a c

-- | Check whether a given element may contain a string.
type family CheckString (a :: Element) where
  CheckString a = If (TestPaternity OnlyText (GetInfo a) (ElementInfo '[FlowContent, PhrasingContent] NoContent NoOmission))
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

newtype Tagged (proxies :: [Symbol]) target (next :: *) = Tagged target

type Symbols a = Fuse (RenderTags (PruneTags (ToTypeList a)))

-- | Retrieve type level meta data about elements.
type family GetInfo a where

  GetInfo DOCTYPE = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo A = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, PalpableContent ]
    (TransparentContent :|: FlowContent :&: NOT InteractiveContent :|: PhrasingContent)
    NoOmission

  GetInfo Abbr = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Address = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (HeadingContent :|: SectioningContent :|: SingleElement Address :|: SingleElement Header :|: SingleElement Footer))
    NoOmission

  GetInfo Area = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent
    RightOmission

  GetInfo Article = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Aside = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Audio = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (SingleElement Source :|: SingleElement Track :|: TransparentContent :&: NOT (SingleElement Audio :|: SingleElement Video))
    NoOmission

  GetInfo B = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Base = ElementInfo
    '[ MetadataContent ]
    NoContent
    RightOmission

  GetInfo Bdi = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Bdo = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Blockquote = ElementInfo
    [ FlowContent, SectioningRoot, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Body = ElementInfo
    '[ SectioningRoot ]
    FlowContent
    NoOmission -- complicated exceptions

  GetInfo Br = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent
    RightOmission

  GetInfo Button = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Canvas = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent ]
    (TransparentContent :&: NOT InteractiveContent :|: SingleElement A :|: SingleElement Button :|: SingleElement Input)
    NoOmission

  GetInfo Caption = ElementInfo
    '[]
    FlowContent
    NoOmission

  GetInfo Cite = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Code = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Col = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo Colgroup = ElementInfo
    '[]
    (SingleElement Col)
    NoOmission -- complicated rules

  GetInfo Data = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Datalist = ElementInfo
    [ FlowContent, PhrasingContent ]
    (PhrasingContent :|: SingleElement Option)
    NoOmission

  GetInfo Dd = ElementInfo
    '[]
    FlowContent
    (LastChildOrFollowedBy '[Dd])

  GetInfo Del = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent
    NoOmission

  GetInfo Details = ElementInfo
    [ FlowContent, SectioningRoot, InteractiveContent, PalpableContent ]
    ( SingleElement Summary :|: FlowContent)
    NoOmission

  GetInfo Dfn = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Dfn))
    NoOmission

  GetInfo Dialog = ElementInfo
    [ FlowContent, SectioningRoot ]
    FlowContent
    NoOmission

  GetInfo Div = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :|: SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template)
    NoOmission

  GetInfo Dl = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Dt :|: SingleElement Dd :|: SingleElement Script :|: SingleElement Template :|: SingleElement Div)
    NoOmission

  GetInfo Dt = ElementInfo
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))
    (LastChildOrFollowedBy '[Dd]) -- really?

  GetInfo Em = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Embed = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    NoContent
    RightOmission

  GetInfo Fieldset = ElementInfo
    [ FlowContent, SectioningRoot, FormAssociatedContent, PalpableContent ]
    (SingleElement Legend :|: FlowContent)
    NoOmission

  GetInfo Figcaption = ElementInfo
    '[]
    FlowContent
    NoOmission

  GetInfo Figure = ElementInfo
    [ FlowContent, SectioningRoot, PalpableContent ]
    (SingleElement Figcaption :|: FlowContent)
    NoOmission

  GetInfo Footer = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Footer :|: SingleElement Header))
    NoOmission

  GetInfo Form = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Form))
    NoOmission

  GetInfo H1 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo H2 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo H3 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo H4 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo H5 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo H6 = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Head = ElementInfo
    '[]
    (MetadataContent :|: SingleElement Title)
    NoOmission -- complicated

  GetInfo Header = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer))
    NoOmission

  GetInfo Hgroup = ElementInfo
    [ FlowContent, HeadingContent, PalpableContent ]
    (SingleElement H1 :|: SingleElement H2 :|: SingleElement H3 :|: SingleElement H4 :|: SingleElement H5 :|: SingleElement H6)
    NoOmission

  GetInfo Hr = ElementInfo
    '[ FlowContent ]
    NoContent
    RightOmission

  GetInfo Html = ElementInfo
    '[]
    (SingleElement Head :|: SingleElement Body)
    NoOmission -- complicated

  GetInfo I = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Iframe = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    NoContent -- complicated
    NoOmission

  GetInfo Img = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent, InteractiveContent ]
    NoContent
    RightOmission

  GetInfo Ins = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent
    NoOmission

  GetInfo Kbd = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Label = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Label))
    NoOmission

  GetInfo Legend = ElementInfo
    '[]
    PhrasingContent
    NoOmission

  GetInfo Li = ElementInfo
    '[]
    FlowContent
    (LastChildOrFollowedBy '[Li]) -- if followed by li or no more content

  GetInfo Link = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    NoContent
    RightOmission

  GetInfo Main = ElementInfo
    [ FlowContent, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Map = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    TransparentContent
    NoOmission

  GetInfo Mark = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Menu = ElementInfo
    [ FlowContent, PalpableContent ]
    (FlowContent :|: SingleElement Li :|: SingleElement Script :|: SingleElement Template :|: SingleElement Menu :|: SingleElement Menuitem :|: SingleElement Hr)
    NoOmission

  GetInfo Menuitem = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo Meta = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    NoContent
    RightOmission

  GetInfo Meter = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Meter))
    NoOmission

  GetInfo Nav = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Noscript = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    (SingleElement Link :|: SingleElement Style :|: SingleElement Meta :|: TransparentContent :&: NOT (SingleElement Noscript) :|: FlowContent :|: PhrasingContent)
    NoOmission

  GetInfo Object = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, PalpableContent, InteractiveContent, FormAssociatedContent ]
    (SingleElement Param :|: TransparentContent)
    NoOmission

  GetInfo Ol = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Li)
    NoOmission

  GetInfo Optgroup = ElementInfo
    '[]
    (SingleElement Option)
    (LastChildOrFollowedBy '[Optgroup])

  GetInfo Option = ElementInfo
    '[]
    OnlyText
    (LastChildOrFollowedBy '[Option, Optgroup])

  GetInfo Output = ElementInfo
    [ FlowContent, PhrasingContent, FormAssociatedContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo P = ElementInfo
    [ FlowContent, PalpableContent ]
    PhrasingContent
    (LastChildOrFollowedBy '[Address, Article, Aside, Blockquote, Div, Dl, Fieldset, Footer, Form, H1, H2, H3, H4, H5, H6, Header, Hr, Menu, Nav, Ol, Pre, Section, Table, Ul, P])
    -- And parent isn't <a>

  GetInfo Param = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo Picture = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent ]
    (SingleElement Source :|: SingleElement Img)
    NoOmission

  GetInfo Pre = ElementInfo
    [ FlowContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Progress = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    (PhrasingContent :&: NOT (SingleElement Progress))
    NoOmission

  GetInfo Q = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Rp = ElementInfo
    '[]
    OnlyText
    NoOmission

  GetInfo Rt = ElementInfo
    '[]
    PhrasingContent
    (LastChildOrFollowedBy '[Rt, Rp])

  GetInfo Rtc = ElementInfo
    '[]
    (PhrasingContent :|: SingleElement Rt)
    (LastChildOrFollowedBy '[Rtc, Rt])

  GetInfo Ruby = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo S = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent
    NoOmission

  GetInfo Samp = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Script = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    OnlyText
    NoOmission

  GetInfo Section = ElementInfo
    [ FlowContent, SectioningContent, PalpableContent ]
    FlowContent
    NoOmission

  GetInfo Select = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent ]
    (SingleElement Option :|: SingleElement Optgroup)
    NoOmission

  GetInfo Slot = ElementInfo
    [ FlowContent, PhrasingContent ]
    TransparentContent
    NoOmission

  GetInfo Small = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent
    NoOmission

  GetInfo Source = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo Span = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent
    NoOmission

  GetInfo Strong = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Style = ElementInfo
    [ MetadataContent, FlowContent ]
    OnlyText
    NoOmission

  GetInfo Sub = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent
    NoOmission

  GetInfo Summary = ElementInfo
    '[]
    (PhrasingContent :|: HeadingContent)
    NoOmission

  GetInfo Sup = ElementInfo
    [ FlowContent, PhrasingContent ]
    PhrasingContent
    NoOmission

  GetInfo Table = ElementInfo
    '[FlowContent]
    (SingleElement Caption :|: SingleElement Colgroup :|: SingleElement Thead :|: SingleElement Tbody :|: SingleElement Tr :|: SingleElement Tfoot)
    NoOmission

  GetInfo Tbody = ElementInfo
    '[]
    (SingleElement Tr)
    NoOmission

  GetInfo Td = ElementInfo
    '[]
    FlowContent
    (LastChildOrFollowedBy '[Th, Td])

  GetInfo Template = ElementInfo
    [ MetadataContent, FlowContent, PhrasingContent ]
    (MetadataContent :|: FlowContent) -- complicated
    NoOmission

  GetInfo Textarea = ElementInfo
    [ FlowContent, PhrasingContent, InteractiveContent, FormAssociatedContent ]
    OnlyText
    NoOmission

  GetInfo Tfoot = ElementInfo
    '[]
    (SingleElement Tr)
    (LastChildOrFollowedBy '[])

  GetInfo Th = ElementInfo
    '[]
    (FlowContent :&: NOT (SingleElement Header :|: SingleElement Footer :|: SectioningContent :|: HeadingContent))
    (LastChildOrFollowedBy '[Th, Td])

  GetInfo Thead = ElementInfo
    '[]
    (SingleElement Tr)
    (LastChildOrFollowedBy '[Tbody, Tfoot])
    -- nearly

  GetInfo Time = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Title = ElementInfo
    '[ MetadataContent ]
    OnlyText
    NoOmission

  GetInfo Tr = ElementInfo
    '[]
    (SingleElement Td :|: SingleElement Th)
    (LastChildOrFollowedBy '[Tr])

  GetInfo Track = ElementInfo
    '[]
    NoContent
    RightOmission

  GetInfo U = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Ul = ElementInfo
    [ FlowContent, PalpableContent ]
    (SingleElement Li)
    NoOmission

  GetInfo Var = ElementInfo
    [ FlowContent, PhrasingContent, PalpableContent ]
    PhrasingContent
    NoOmission

  GetInfo Video = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (SingleElement Track :|: TransparentContent :&: NOT (SingleElement Audio :|: SingleElement Video) :|: SingleElement Source)
    NoOmission

  GetInfo Wbr = ElementInfo
    [ FlowContent, PhrasingContent ]
    NoContent
    RightOmission

  GetInfo _ = ElementInfo
    [ FlowContent, PhrasingContent, EmbeddedContent, InteractiveContent, PalpableContent ]
    (FlowContent :|: PhrasingContent :|: EmbeddedContent :|: InteractiveContent :|: PalpableContent)
    NoOmission
