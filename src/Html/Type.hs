{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}

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
  = A
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
  a ?> Maybe b    = a ?> b
  a ?> Either b c = (a ?> b, a ?> c)
  a ?> f (b > c)  = a ?> (b > c)
  a ?> f (b # c)  = a ?> (b # c)
  a ?> ()         = ()
  a ?> (b -> c)   = (TypeError (Text "Html elements can't contain functions"))
  a ?> b          = CheckString a


-- | Combine two elements sequentially.
--
-- >>> render (i_ () # div_ ()) :: String
-- "<i></i><div></div>"
data (#) a b = (:#:) a b
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
--  WithAttributes :: (a ?> b) => [(String, String)] -> b -> a > b

infixr 8 >

  -------------------
  -- internal code --
  -------------------

-- | These tags will get cleaned up with ghc8.2 AppendSymbol
type family OpenTag e where
  OpenTag A          = "<a>"
  OpenTag Abbr       = "<abbr>"
  OpenTag Acronym    = "<acronym>"
  OpenTag Address    = "<address>"
  OpenTag Applet     = "<applet>"
  OpenTag Area       = "<area>"
  OpenTag Article    = "<article>"
  OpenTag Aside      = "<aside>"
  OpenTag Audio      = "<audio>"
  OpenTag B          = "<b>"
  OpenTag Base       = "<base>"
  OpenTag Basefont   = "<basefont>"
  OpenTag Bdi        = "<bdi>"
  OpenTag Bdo        = "<bdo>"
  OpenTag Bgsound    = "<bgsound>"
  OpenTag Big        = "<big>"
  OpenTag Blink      = "<blink>"
  OpenTag Blockquote = "<blockquote>"
  OpenTag Body       = "<body>"
  OpenTag Br         = "<br>"
  OpenTag Button     = "<button>"
  OpenTag Canvas     = "<canvas>"
  OpenTag Caption    = "<caption>"
  OpenTag Center     = "<center>"
  OpenTag Cite       = "<cite>"
  OpenTag Code       = "<code>"
  OpenTag Col        = "<col>"
  OpenTag Colgroup   = "<colgroup>"
  OpenTag Command    = "<command>"
  OpenTag Content    = "<content>"
  OpenTag Data       = "<data>"
  OpenTag Datalist   = "<datalist>"
  OpenTag Dd         = "<dd>"
  OpenTag Del        = "<del>"
  OpenTag Details    = "<details>"
  OpenTag Dfn        = "<dfn>"
  OpenTag Dialog     = "<dialog>"
  OpenTag Dir        = "<dir>"
  OpenTag Div        = "<div>"
  OpenTag Dl         = "<dl>"
  OpenTag Dt         = "<dt>"
  OpenTag 'Element   = "<element>"
  OpenTag Em         = "<em>"
  OpenTag Embed      = "<embed>"
  OpenTag Fieldset   = "<fieldset>"
  OpenTag Figcaption = "<figcaption>"
  OpenTag Figure     = "<figure>"
  OpenTag Font       = "<font>"
  OpenTag Footer     = "<footer>"
  OpenTag Form       = "<form>"
  OpenTag Frame      = "<frame>"
  OpenTag Frameset   = "<frameset>"
  OpenTag H1         = "<h1>"
  OpenTag H2         = "<h2>"
  OpenTag H3         = "<h3>"
  OpenTag H4         = "<h4>"
  OpenTag H5         = "<h5>"
  OpenTag H6         = "<h6>"
  OpenTag Head       = "<head>"
  OpenTag Header     = "<header>"
  OpenTag Hgroup     = "<hgroup>"
  OpenTag Hr         = "<hr>"
  OpenTag Html       = "<html>"
  OpenTag I          = "<i>"
  OpenTag Iframe     = "<iframe>"
  OpenTag Image      = "<image>"
  OpenTag Img        = "<img>"
  OpenTag Input      = "<input>"
  OpenTag Ins        = "<ins>"
  OpenTag Isindex    = "<isindex>"
  OpenTag Kbd        = "<kbd>"
  OpenTag Keygen     = "<keygen>"
  OpenTag Label      = "<label>"
  OpenTag Legend     = "<legend>"
  OpenTag Li         = "<li>"
  OpenTag Link       = "<link>"
  OpenTag Listing    = "<listing>"
  OpenTag Main       = "<main>"
  OpenTag Map        = "<map>"
  OpenTag Mark       = "<mark>"
  OpenTag Marquee    = "<marquee>"
  OpenTag Math       = "<math>"
  OpenTag Menu       = "<menu>"
  OpenTag Menuitem   = "<menuitem>"
  OpenTag Meta       = "<meta>"
  OpenTag Meter      = "<meter>"
  OpenTag Multicol   = "<multicol>"
  OpenTag Nav        = "<nav>"
  OpenTag Nextid     = "<nextid>"
  OpenTag Nobr       = "<nobr>"
  OpenTag Noembed    = "<noembed>"
  OpenTag Noframes   = "<noframes>"
  OpenTag Noscript   = "<noscript>"
  OpenTag Object     = "<object>"
  OpenTag Ol         = "<ol>"
  OpenTag Optgroup   = "<optgroup>"
  OpenTag Option     = "<option>"
  OpenTag Output     = "<output>"
  OpenTag P          = "<p>"
  OpenTag Param      = "<param>"
  OpenTag Picture    = "<picture>"
  OpenTag Plaintext  = "<plaintext>"
  OpenTag Pre        = "<pre>"
  OpenTag Progress   = "<progress>"
  OpenTag Q          = "<q>"
  OpenTag Rp         = "<rp>"
  OpenTag Rt         = "<rt>"
  OpenTag Rtc        = "<rtc>"
  OpenTag Ruby       = "<ruby>"
  OpenTag S          = "<s>"
  OpenTag Samp       = "<samp>"
  OpenTag Script     = "<script>"
  OpenTag Section    = "<section>"
  OpenTag Select     = "<select>"
  OpenTag Shadow     = "<shadow>"
  OpenTag Slot       = "<slot>"
  OpenTag Small      = "<small>"
  OpenTag Source     = "<source>"
  OpenTag Spacer     = "<spacer>"
  OpenTag Span       = "<span>"
  OpenTag Strike     = "<strike>"
  OpenTag Strong     = "<strong>"
  OpenTag Style      = "<style>"
  OpenTag Sub        = "<sub>"
  OpenTag Summary    = "<summary>"
  OpenTag Sup        = "<sup>"
  OpenTag Svg        = "<svg>"
  OpenTag Table      = "<table>"
  OpenTag Tbody      = "<tbody>"
  OpenTag Td         = "<td>"
  OpenTag Template   = "<template>"
  OpenTag Textarea   = "<textarea>"
  OpenTag Tfoot      = "<tfoot>"
  OpenTag Th         = "<th>"
  OpenTag Thead      = "<thead>"
  OpenTag Time       = "<time>"
  OpenTag Title      = "<title>"
  OpenTag Tr         = "<tr>"
  OpenTag Track      = "<track>"
  OpenTag Tt         = "<tt>"
  OpenTag U          = "<u>"
  OpenTag Ul         = "<ul>"
  OpenTag Var        = "<var>"
  OpenTag Video      = "<video>"
  OpenTag Wbr        = "<wbr>"
  OpenTag Xmp        = "<xmp>"

type family CloseTag e where
  CloseTag A          = "</a>"
  CloseTag Abbr       = "</abbr>"
  CloseTag Acronym    = "</acronym>"
  CloseTag Address    = "</address>"
  CloseTag Applet     = "</applet>"
  CloseTag Area       = "</area>"
  CloseTag Article    = "</article>"
  CloseTag Aside      = "</aside>"
  CloseTag Audio      = "</audio>"
  CloseTag B          = "</b>"
  CloseTag Base       = "</base>"
  CloseTag Basefont   = "</basefont>"
  CloseTag Bdi        = "</bdi>"
  CloseTag Bdo        = "</bdo>"
  CloseTag Bgsound    = "</bgsound>"
  CloseTag Big        = "</big>"
  CloseTag Blink      = "</blink>"
  CloseTag Blockquote = "</blockquote>"
  CloseTag Body       = "</body>"
  CloseTag Br         = "</br>"
  CloseTag Button     = "</button>"
  CloseTag Canvas     = "</canvas>"
  CloseTag Caption    = "</caption>"
  CloseTag Center     = "</center>"
  CloseTag Cite       = "</cite>"
  CloseTag Code       = "</code>"
  CloseTag Col        = "</col>"
  CloseTag Colgroup   = "</colgroup>"
  CloseTag Command    = "</command>"
  CloseTag Content    = "</content>"
  CloseTag Data       = "</data>"
  CloseTag Datalist   = "</datalist>"
  CloseTag Dd         = "</dd>"
  CloseTag Del        = "</del>"
  CloseTag Details    = "</details>"
  CloseTag Dfn        = "</dfn>"
  CloseTag Dialog     = "</dialog>"
  CloseTag Dir        = "</dir>"
  CloseTag Div        = "</div>"
  CloseTag Dl         = "</dl>"
  CloseTag Dt         = "</dt>"
  CloseTag 'Element   = "</element>"
  CloseTag Em         = "</em>"
  CloseTag Embed      = "</embed>"
  CloseTag Fieldset   = "</fieldset>"
  CloseTag Figcaption = "</figcaption>"
  CloseTag Figure     = "</figure>"
  CloseTag Font       = "</font>"
  CloseTag Footer     = "</footer>"
  CloseTag Form       = "</form>"
  CloseTag Frame      = "</frame>"
  CloseTag Frameset   = "</frameset>"
  CloseTag H1         = "</h1>"
  CloseTag H2         = "</h2>"
  CloseTag H3         = "</h3>"
  CloseTag H4         = "</h4>"
  CloseTag H5         = "</h5>"
  CloseTag H6         = "</h6>"
  CloseTag Head       = "</head>"
  CloseTag Header     = "</header>"
  CloseTag Hgroup     = "</hgroup>"
  CloseTag Hr         = "</hr>"
  CloseTag Html       = "</html>"
  CloseTag I          = "</i>"
  CloseTag Iframe     = "</iframe>"
  CloseTag Image      = "</image>"
  CloseTag Img        = "</img>"
  CloseTag Input      = "</input>"
  CloseTag Ins        = "</ins>"
  CloseTag Isindex    = "</isindex>"
  CloseTag Kbd        = "</kbd>"
  CloseTag Keygen     = "</keygen>"
  CloseTag Label      = "</label>"
  CloseTag Legend     = "</legend>"
  CloseTag Li         = "</li>"
  CloseTag Link       = "</link>"
  CloseTag Listing    = "</listing>"
  CloseTag Main       = "</main>"
  CloseTag Map        = "</map>"
  CloseTag Mark       = "</mark>"
  CloseTag Marquee    = "</marquee>"
  CloseTag Math       = "</math>"
  CloseTag Menu       = "</menu>"
  CloseTag Menuitem   = "</menuitem>"
  CloseTag Meta       = "</meta>"
  CloseTag Meter      = "</meter>"
  CloseTag Multicol   = "</multicol>"
  CloseTag Nav        = "</nav>"
  CloseTag Nextid     = "</nextid>"
  CloseTag Nobr       = "</nobr>"
  CloseTag Noembed    = "</noembed>"
  CloseTag Noframes   = "</noframes>"
  CloseTag Noscript   = "</noscript>"
  CloseTag Object     = "</object>"
  CloseTag Ol         = "</ol>"
  CloseTag Optgroup   = "</optgroup>"
  CloseTag Option     = "</option>"
  CloseTag Output     = "</output>"
  CloseTag P          = "</p>"
  CloseTag Param      = "</param>"
  CloseTag Picture    = "</picture>"
  CloseTag Plaintext  = "</plaintext>"
  CloseTag Pre        = "</pre>"
  CloseTag Progress   = "</progress>"
  CloseTag Q          = "</q>"
  CloseTag Rp         = "</rp>"
  CloseTag Rt         = "</rt>"
  CloseTag Rtc        = "</rtc>"
  CloseTag Ruby       = "</ruby>"
  CloseTag S          = "</s>"
  CloseTag Samp       = "</samp>"
  CloseTag Script     = "</script>"
  CloseTag Section    = "</section>"
  CloseTag Select     = "</select>"
  CloseTag Shadow     = "</shadow>"
  CloseTag Slot       = "</slot>"
  CloseTag Small      = "</small>"
  CloseTag Source     = "</source>"
  CloseTag Spacer     = "</spacer>"
  CloseTag Span       = "</span>"
  CloseTag Strike     = "</strike>"
  CloseTag Strong     = "</strong>"
  CloseTag Style      = "</style>"
  CloseTag Sub        = "</sub>"
  CloseTag Summary    = "</summary>"
  CloseTag Sup        = "</sup>"
  CloseTag Svg        = "</svg>"
  CloseTag Table      = "</table>"
  CloseTag Tbody      = "</tbody>"
  CloseTag Td         = "</td>"
  CloseTag Template   = "</template>"
  CloseTag Textarea   = "</textarea>"
  CloseTag Tfoot      = "</tfoot>"
  CloseTag Th         = "</th>"
  CloseTag Thead      = "</thead>"
  CloseTag Time       = "</time>"
  CloseTag Title      = "</title>"
  CloseTag Tr         = "</tr>"
  CloseTag Track      = "</track>"
  CloseTag Tt         = "</tt>"
  CloseTag U          = "</u>"
  CloseTag Ul         = "</ul>"
  CloseTag Var        = "</var>"
  CloseTag Video      = "</video>"
  CloseTag Wbr        = "</wbr>"
  CloseTag Xmp        = "</xmp>"

-- | Flatten a html tree of elements into a type list of tags.
type family ToTypeList a where
  ToTypeList (a # b)  = Append (ToTypeList a) (ToTypeList b)
  ToTypeList (a > ()) = If (HasContent (GetInfo a)) (Open a, Close a) (Open a)
  ToTypeList (a > b)  = Append (Open a, ToTypeList b) (Close a)
  ToTypeList [a # b]  = [ToTypeList (a # b)]
  ToTypeList [a > b]  = [ToTypeList (a > b)]
  ToTypeList x        = x

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
  PruneTags [a]          = [PruneTags a]
  PruneTags (Close a, b) = IsOmittable (GetInfo a) (Close a, b)
  PruneTags ([a],b)      = IsOmittableL (Head' (PruneTags a)) [PruneTags a] (Last (PruneTags a)) b
  PruneTags (a, b)       = (a, PruneTags b)
  PruneTags a            = a

type family IsOmittableL head list last next where
  IsOmittableL (Open head) list (Close last) (Close next) = IsOmittable2 (Open head) list (GetInfo last) (Close next)

-- Base case
  IsOmittableL _head list _last next = (list, PruneTags next)

type family IsOmittable2 head list last next where
  IsOmittable2
    (Open head)
    [list]
    (ElementInfo _ _ (LastChildOrFollowedBy (head ': _))) -- last
    (Close next)
    = ([Init list], PruneTags (Close next))

  IsOmittable2
    (Open head)
    [list]
    (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) -- last
    (Close next)
    = IsOmittable2 (Open head) [list] (ElementInfo a b (LastChildOrFollowedBy xs)) (Close next)

  IsOmittable2
    (Open head)
    [list]
    _last
    (Close next)
    = ([list], PruneTags (Close next))

-- Base case
  IsOmittable2
    _head
    list
    _last
    next
    = (list, PruneTags next)


-- | Checks whether a tag is omittable.  Sadly, returning a kind Bool
-- would make the compiler loop, so we inline the if into the type
-- function.
type family IsOmittable a b where
  IsOmittable (ElementInfo _ _ RightOmission) (_,c)                               = PruneTags c
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy _)) (_,(Close b, c))        = PruneTags (Close b, c)
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy _)) (_,Close b)             = Close b
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy '[])) (b,c)                 = (b, PruneTags c)
  IsOmittable (ElementInfo _ _ (LastChildOrFollowedBy (x ': _))) (c, (Open x, d)) = PruneTags (Open x, d)
  IsOmittable (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) c               = IsOmittable (ElementInfo a b (LastChildOrFollowedBy xs)) c
  IsOmittable _ (c,d)                                                             = (c, PruneTags d)

-- | Convert tags to type level strings.
type family RenderTags a where
  RenderTags [a]       = [RenderTags a]
  RenderTags (a, b)    = (RenderTags a, RenderTags b)
  RenderTags (Open a)  = Proxy (OpenTag a)
  RenderTags (Close a) = Proxy (CloseTag a)
  RenderTags EndOfOpen = Proxy ">"
  RenderTags a         = a

-- | Fuse neighbouring type level strings.
type family Fuse a where
  Fuse (Proxy (a :: Symbol), (Proxy (b :: Symbol), c)) = Fuse (Proxy "{FUSED-A}", c)
  Fuse (Proxy (a :: Symbol), Proxy (b :: Symbol))      = Proxy "{FUSED-B}"
  Fuse (Proxy (a :: Symbol), (b,c))                    = (Proxy a, Fuse c)
  Fuse (Proxy (a :: Symbol), b)                        = (Proxy a, Fuse b)
  Fuse (Proxy (a :: Symbol))                           = Proxy a
  Fuse (a, b)                                          = (Proxy "", Fuse b)
  Fuse a                                               = Proxy ""

-- | Init for type level lists.
type family Init xs where
  Init (a, (b,c)) = (a, Init (b, c))
  Init (a, b)     = a
  Init a          = a

-- | Last for type level lists.
type family Last a where
  Last (a, as) = Last as
  Last a       = a

-- | Last for type level lists.
type family Head' a where
  Head' (a, as) = a
  Head' a       = a

-- | Utility types.
data EndOfOpen
data Open (a :: Element)
data Close (a :: Element)
newtype Attribute = Attribute [(String, String)]

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

type family Rep n x where
  Rep 0 _ = TypeError (Text "Can't replicate 0 times")
  Rep 1 x = x
  Rep n x = x # Rep (n-1) x

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

newtype Tagged target (next :: *) = Tagged target

-- | Retrieve type level meta data about elements.
type family GetInfo a where

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
