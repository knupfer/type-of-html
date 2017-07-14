{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Html where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Bool
import Data.Monoid

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8

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

 "This is an obsolete API and is no longer guaranteed to work." #-}

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
  deriving (Show, Enum, Bounded)

type family OpenTag e where
  OpenTag A = "<a>"
  OpenTag Abbr = "<abbr>"
  OpenTag Acronym = "<acronym>"
  OpenTag Address = "<address>"
  OpenTag Applet = "<applet>"
  OpenTag Area = "<area>"
  OpenTag Article = "<article>"
  OpenTag Aside = "<aside>"
  OpenTag Audio = "<audio>"
  OpenTag B = "<b>"
  OpenTag Base = "<base>"
  OpenTag Basefont = "<basefont>"
  OpenTag Bdi = "<bdi>"
  OpenTag Bdo = "<bdo>"
  OpenTag Bgsound = "<bgsound>"
  OpenTag Big = "<big>"
  OpenTag Blink = "<blink>"
  OpenTag Blockquote = "<blockquote>"
  OpenTag Body = "<body>"
  OpenTag Br = "<br>"
  OpenTag Button = "<button>"
  OpenTag Canvas = "<canvas>"
  OpenTag Caption = "<caption>"
  OpenTag Center = "<center>"
  OpenTag Cite = "<cite>"
  OpenTag Code = "<code>"
  OpenTag Col = "<col>"
  OpenTag Colgroup = "<colgroup>"
  OpenTag Command = "<command>"
  OpenTag Content = "<content>"
  OpenTag Data = "<data>"
  OpenTag Datalist = "<datalist>"
  OpenTag Dd = "<dd>"
  OpenTag Del = "<del>"
  OpenTag Details = "<details>"
  OpenTag Dfn = "<dfn>"
  OpenTag Dialog = "<dialog>"
  OpenTag Dir = "<dir>"
  OpenTag Div = "<div>"
  OpenTag Dl = "<dl>"
  OpenTag Dt = "<dt>"
  OpenTag 'Element = "<element>"
  OpenTag Em = "<em>"
  OpenTag Embed = "<embed>"
  OpenTag Fieldset = "<fieldset>"
  OpenTag Figcaption = "<figcaption>"
  OpenTag Figure = "<figure>"
  OpenTag Font = "<font>"
  OpenTag Footer = "<footer>"
  OpenTag Form = "<form>"
  OpenTag Frame = "<frame>"
  OpenTag Frameset = "<frameset>"
  OpenTag H1 = "<h1>"
  OpenTag H2 = "<h2>"
  OpenTag H3 = "<h3>"
  OpenTag H4 = "<h4>"
  OpenTag H5 = "<h5>"
  OpenTag H6 = "<h6>"
  OpenTag Head = "<head>"
  OpenTag Header = "<header>"
  OpenTag Hgroup = "<hgroup>"
  OpenTag Hr = "<hr>"
  OpenTag Html = "<html>"
  OpenTag I = "<i>"
  OpenTag Iframe = "<iframe>"
  OpenTag Image = "<image>"
  OpenTag Img = "<img>"
  OpenTag Input = "<input>"
  OpenTag Ins = "<ins>"
  OpenTag Isindex = "<isindex>"
  OpenTag Kbd = "<kbd>"
  OpenTag Keygen = "<keygen>"
  OpenTag Label = "<label>"
  OpenTag Legend = "<legend>"
  OpenTag Li = "<li>"
  OpenTag Link = "<link>"
  OpenTag Listing = "<listing>"
  OpenTag Main = "<main>"
  OpenTag Map = "<map>"
  OpenTag Mark = "<mark>"
  OpenTag Marquee = "<marquee>"
  OpenTag Math = "<math>"
  OpenTag Menu = "<menu>"
  OpenTag Menuitem = "<menuitem>"
  OpenTag Meta = "<meta>"
  OpenTag Meter = "<meter>"
  OpenTag Multicol = "<multicol>"
  OpenTag Nav = "<nav>"
  OpenTag Nextid = "<nextid>"
  OpenTag Nobr = "<nobr>"
  OpenTag Noembed = "<noembed>"
  OpenTag Noframes = "<noframes>"
  OpenTag Noscript = "<noscript>"
  OpenTag Object = "<object>"
  OpenTag Ol = "<ol>"
  OpenTag Optgroup = "<optgroup>"
  OpenTag Option = "<option>"
  OpenTag Output = "<output>"
  OpenTag P = "<p>"
  OpenTag Param = "<param>"
  OpenTag Picture = "<picture>"
  OpenTag Plaintext = "<plaintext>"
  OpenTag Pre = "<pre>"
  OpenTag Progress = "<progress>"
  OpenTag Q = "<q>"
  OpenTag Rp = "<rp>"
  OpenTag Rt = "<rt>"
  OpenTag Rtc = "<rtc>"
  OpenTag Ruby = "<ruby>"
  OpenTag S = "<s>"
  OpenTag Samp = "<samp>"
  OpenTag Script = "<script>"
  OpenTag Section = "<section>"
  OpenTag Select = "<select>"
  OpenTag Shadow = "<shadow>"
  OpenTag Slot = "<slot>"
  OpenTag Small = "<small>"
  OpenTag Source = "<source>"
  OpenTag Spacer = "<spacer>"
  OpenTag Span = "<span>"
  OpenTag Strike = "<strike>"
  OpenTag Strong = "<strong>"
  OpenTag Style = "<style>"
  OpenTag Sub = "<sub>"
  OpenTag Summary = "<summary>"
  OpenTag Sup = "<sup>"
  OpenTag Svg = "<svg>"
  OpenTag Table = "<table>"
  OpenTag Tbody = "<tbody>"
  OpenTag Td = "<td>"
  OpenTag Template = "<template>"
  OpenTag Textarea = "<textarea>"
  OpenTag Tfoot = "<tfoot>"
  OpenTag Th = "<th>"
  OpenTag Thead = "<thead>"
  OpenTag Time = "<time>"
  OpenTag Title = "<title>"
  OpenTag Tr = "<tr>"
  OpenTag Track = "<track>"
  OpenTag Tt = "<tt>"
  OpenTag U = "<u>"
  OpenTag Ul = "<ul>"
  OpenTag Var = "<var>"
  OpenTag Video = "<video>"
  OpenTag Wbr = "<wbr>"
  OpenTag Xmp = "<xmp>"

type family CloseTag e where
  CloseTag A = "</a>"
  CloseTag Abbr = "</abbr>"
  CloseTag Acronym = "</acronym>"
  CloseTag Address = "</address>"
  CloseTag Applet = "</applet>"
  CloseTag Area = "</area>"
  CloseTag Article = "</article>"
  CloseTag Aside = "</aside>"
  CloseTag Audio = "</audio>"
  CloseTag B = "</b>"
  CloseTag Base = "</base>"
  CloseTag Basefont = "</basefont>"
  CloseTag Bdi = "</bdi>"
  CloseTag Bdo = "</bdo>"
  CloseTag Bgsound = "</bgsound>"
  CloseTag Big = "</big>"
  CloseTag Blink = "</blink>"
  CloseTag Blockquote = "</blockquote>"
  CloseTag Body = "</body>"
  CloseTag Br = "</br>"
  CloseTag Button = "</button>"
  CloseTag Canvas = "</canvas>"
  CloseTag Caption = "</caption>"
  CloseTag Center = "</center>"
  CloseTag Cite = "</cite>"
  CloseTag Code = "</code>"
  CloseTag Col = "</col>"
  CloseTag Colgroup = "</colgroup>"
  CloseTag Command = "</command>"
  CloseTag Content = "</content>"
  CloseTag Data = "</data>"
  CloseTag Datalist = "</datalist>"
  CloseTag Dd = "</dd>"
  CloseTag Del = "</del>"
  CloseTag Details = "</details>"
  CloseTag Dfn = "</dfn>"
  CloseTag Dialog = "</dialog>"
  CloseTag Dir = "</dir>"
  CloseTag Div = "</div>"
  CloseTag Dl = "</dl>"
  CloseTag Dt = "</dt>"
  CloseTag 'Element = "</element>"
  CloseTag Em = "</em>"
  CloseTag Embed = "</embed>"
  CloseTag Fieldset = "</fieldset>"
  CloseTag Figcaption = "</figcaption>"
  CloseTag Figure = "</figure>"
  CloseTag Font = "</font>"
  CloseTag Footer = "</footer>"
  CloseTag Form = "</form>"
  CloseTag Frame = "</frame>"
  CloseTag Frameset = "</frameset>"
  CloseTag H1 = "</h1>"
  CloseTag H2 = "</h2>"
  CloseTag H3 = "</h3>"
  CloseTag H4 = "</h4>"
  CloseTag H5 = "</h5>"
  CloseTag H6 = "</h6>"
  CloseTag Head = "</head>"
  CloseTag Header = "</header>"
  CloseTag Hgroup = "</hgroup>"
  CloseTag Hr = "</hr>"
  CloseTag Html = "</html>"
  CloseTag I = "</i>"
  CloseTag Iframe = "</iframe>"
  CloseTag Image = "</image>"
  CloseTag Img = "</img>"
  CloseTag Input = "</input>"
  CloseTag Ins = "</ins>"
  CloseTag Isindex = "</isindex>"
  CloseTag Kbd = "</kbd>"
  CloseTag Keygen = "</keygen>"
  CloseTag Label = "</label>"
  CloseTag Legend = "</legend>"
  CloseTag Li = "</li>"
  CloseTag Link = "</link>"
  CloseTag Listing = "</listing>"
  CloseTag Main = "</main>"
  CloseTag Map = "</map>"
  CloseTag Mark = "</mark>"
  CloseTag Marquee = "</marquee>"
  CloseTag Math = "</math>"
  CloseTag Menu = "</menu>"
  CloseTag Menuitem = "</menuitem>"
  CloseTag Meta = "</meta>"
  CloseTag Meter = "</meter>"
  CloseTag Multicol = "</multicol>"
  CloseTag Nav = "</nav>"
  CloseTag Nextid = "</nextid>"
  CloseTag Nobr = "</nobr>"
  CloseTag Noembed = "</noembed>"
  CloseTag Noframes = "</noframes>"
  CloseTag Noscript = "</noscript>"
  CloseTag Object = "</object>"
  CloseTag Ol = "</ol>"
  CloseTag Optgroup = "</optgroup>"
  CloseTag Option = "</option>"
  CloseTag Output = "</output>"
  CloseTag P = "</p>"
  CloseTag Param = "</param>"
  CloseTag Picture = "</picture>"
  CloseTag Plaintext = "</plaintext>"
  CloseTag Pre = "</pre>"
  CloseTag Progress = "</progress>"
  CloseTag Q = "</q>"
  CloseTag Rp = "</rp>"
  CloseTag Rt = "</rt>"
  CloseTag Rtc = "</rtc>"
  CloseTag Ruby = "</ruby>"
  CloseTag S = "</s>"
  CloseTag Samp = "</samp>"
  CloseTag Script = "</script>"
  CloseTag Section = "</section>"
  CloseTag Select = "</select>"
  CloseTag Shadow = "</shadow>"
  CloseTag Slot = "</slot>"
  CloseTag Small = "</small>"
  CloseTag Source = "</source>"
  CloseTag Spacer = "</spacer>"
  CloseTag Span = "</span>"
  CloseTag Strike = "</strike>"
  CloseTag Strong = "</strong>"
  CloseTag Style = "</style>"
  CloseTag Sub = "</sub>"
  CloseTag Summary = "</summary>"
  CloseTag Sup = "</sup>"
  CloseTag Svg = "</svg>"
  CloseTag Table = "</table>"
  CloseTag Tbody = "</tbody>"
  CloseTag Td = "</td>"
  CloseTag Template = "</template>"
  CloseTag Textarea = "</textarea>"
  CloseTag Tfoot = "</tfoot>"
  CloseTag Th = "</th>"
  CloseTag Thead = "</thead>"
  CloseTag Time = "</time>"
  CloseTag Title = "</title>"
  CloseTag Tr = "</tr>"
  CloseTag Track = "</track>"
  CloseTag Tt = "</tt>"
  CloseTag U = "</u>"
  CloseTag Ul = "</ul>"
  CloseTag Var = "</var>"
  CloseTag Video = "</video>"
  CloseTag Wbr = "</wbr>"
  CloseTag Xmp = "</xmp>"

type family Flatten a where
  Flatten (a # b) = Combine (Flatten a) (Flatten b)
  Flatten (a > ())= If (HasNoContent (GetInfo a)) (Open a) (Open a, Close a)
  Flatten (a > b) = Combine (Open a, Flatten b) (Close a)
  Flatten [a # b] = [Flatten (a # b)]
  Flatten [a > b] = [Flatten (a > b)]
  Flatten (Helper _ a) = Flatten a
  Flatten x = x

type family Combine a b where
  Combine (a, b) c = (a, Combine b c)
  Combine a b = (a, b)

type family HasNoContent a where
  HasNoContent (ElementInfo _ NoContent _) = True
  HasNoContent _ = False

type family PruneTags a where
  PruneTags ((), a) = PruneTags a
  PruneTags (a , ()) = PruneTags a
  PruneTags [a] = [PruneTags a]
  PruneTags (Close a, b) = IsOmitable' (GetInfo a) (Close a, b)
  PruneTags (a, b) = (a, PruneTags b)
  PruneTags (Helper a b) = PruneTags b
  PruneTags a = a

type family IsOmitable' a b where
  IsOmitable' (ElementInfo _ _ RightOmission) (_,c) = PruneTags c
  IsOmitable' (ElementInfo _ _ (LastChildOrFollowedBy _)) (_,(Close b, c)) = PruneTags(Close b, c)
  IsOmitable' (ElementInfo _ _ (LastChildOrFollowedBy '[])) (b,c) = (b, PruneTags c)
  IsOmitable' (ElementInfo _ _ (LastChildOrFollowedBy (x ': _))) (c, (Open x, d)) = PruneTags(Open x, d)
  IsOmitable' (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) c =
      IsOmitable' (ElementInfo a b (LastChildOrFollowedBy xs)) c
  IsOmitable' _ (c,d) = (c, PruneTags d)

type family IsOmitable a b where
  IsOmitable (ElementInfo _ _ RightOmission) _ = True
  IsOmitable (ElementInfo _ _ (LastChildOrFollowedBy _)) (Close _) = True
  IsOmitable (ElementInfo _ _ (LastChildOrFollowedBy '[])) _ = False
  IsOmitable (ElementInfo _ _ (LastChildOrFollowedBy (x ': _))) (Open x) = True
  IsOmitable (ElementInfo a b (LastChildOrFollowedBy (_ ': xs))) (Open x) =
      IsOmitable (ElementInfo a b (LastChildOrFollowedBy xs)) (Open x)
  IsOmitable _ _ = False

type family RenderRecursive a where
  RenderRecursive [a] = [RenderRecursive a]
  RenderRecursive (a, b) = (RenderRecursive a, RenderRecursive b)
  RenderRecursive a = RenderTag a

type family RenderTag a where
  RenderTag (Open a) = Proxy (OpenTag a)
  RenderTag (Close a) = Proxy (CloseTag a)
  RenderTag EndOfOpen = Proxy ">"
  RenderTag a = a

type family Fuse a where
  Fuse (Proxy (a :: Symbol), (Proxy (b :: Symbol), c)) = Fuse (Proxy "FUSED", c)
  Fuse (Proxy (a :: Symbol), Proxy (b :: Symbol))      = Proxy "FUSED"
  Fuse (Proxy (a :: Symbol), (b, c))                   = (Proxy a, Fuse c)
  Fuse (a, b)                                          = (Proxy "", Fuse b)
  Fuse (Proxy (a :: Symbol))                           = Proxy a
  Fuse a                                               = Proxy ""

type Render html string
  = ( IsString string
    , FlatR
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
      ) string
    , FlatR
       ( Init
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
        )
      ) string
    , FlatR
      ( Last'
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
        )
      ) string
    , FlatR html string
    , Monoid string
    )

type family Init xs where
  Init (a, (b,c)) = (a, Init (b, c))
  Init (a, b) = a
  Init a = a

type family Last' a where
  Last' (a, as) = Last' as
  Last' a = a

{-# INLINE render #-}
render :: Render a b => a -> b
render = mconcat . renderList

{-# NOINLINE renderList #-}
renderList :: Render a b => a -> [b]
renderList = renderList_

{-# INLINE renderListB #-}
renderListB :: Render a TLB.Builder => a -> [TLB.Builder]
renderListB x =
    augment (\c n -> foldr2_ (zipWithFB_ c (<>)) n elements contents) closing
  where contents = flatR x
        elements = flatR (f x)
        closing = flatR (g x)
        f :: x -> Init (Fuse (RenderRecursive (PruneTags (Flatten x))))
        f = undefined
        g :: x -> Last' (Fuse (RenderRecursive (PruneTags (Flatten x))))
        g = undefined

{-# INLINE renderList_ #-}
renderList_ :: Render a b => a -> [b]
renderList_ x = g elements contents
  where contents = flatR x
        elements = flatR (f x)
        f :: x -> Fuse (RenderRecursive (PruneTags (Flatten x)))
        f = undefined
        g (a:as) (b:bs) = a:b:g as bs
        g as [] = as
        g [] bs = bs

{-# INLINE renderListLT #-}
renderListLT :: Render a LT.Text => a -> [LT.Text]
renderListLT = renderList_

{-# INLINE renderListT #-}
renderListT :: Render a T.Text => a -> [T.Text]
renderListT = renderList_

{-# INLINE renderListS #-}
renderListS :: Render a String => a -> [String]
renderListS = renderList_

{-# RULES
"renderList/builder"     renderList = renderListB
"renderList/lazy text"   renderList = renderListLT
"renderList/strict text" renderList = renderListT
"renderList/string"      renderList = renderListS
  #-}

{-# INLINE [0] zipWithFB_ #-}
zipWithFB_ :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB_ c f = \x y r -> (x `f` y) `c` r

{-# INLINE [0] foldr2_ #-}
foldr2_ :: Monoid a => (a -> a -> c -> c) -> c -> [a] -> [a] -> c
foldr2_ k z = go
  where
        go []     _      = z
        go _      []     = z
        go (x:xs) (y:ys) = k x y (go xs ys)

foldr2_left_ :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left_ _k  z _x _r []     = z
foldr2_left_  k _z  x  r (y:ys) = k x y (r ys)

{-# RULES
"foldr2/left_"   forall k z ys (g:: forall b.(a->b->b)->b->b) .
                  foldr2_ k z (build g) ys = g (foldr2_left_  k z) (\_ -> z) ys
 #-}

class FlatR a b where
  flatR :: (IsString b, Monoid b) => a -> [b]

instance (KnownSymbol a, FlatR b str) => FlatR (Proxy a, b) str where
  {-# INLINE flatR #-}
  flatR _ = doRender (Proxy :: Proxy a):flatR (undefined :: b)

instance KnownSymbol a => FlatR (Proxy a) str where
  {-# INLINE flatR #-}
  flatR _ = [doRender (Proxy :: Proxy a)]

instance {-# OVERLAPPABLE #-} DoRender a str => FlatR a str where
  {-# INLINE flatR #-}
  flatR x = [doRender x]

instance FlatR () str where
  {-# INLINE flatR #-}
  flatR _ = []

instance (FlatR a str, FlatR b str) => FlatR (a # b) str where
  {-# INLINE flatR #-}
  flatR (a :#: b) = flatR a ++ flatR b

instance FlatR b str => FlatR (a > b) str where
  {-# INLINE flatR #-}
  flatR (Child x) = flatR x

instance (FlatR (a > b) str, FlatR (Fuse (RenderRecursive (PruneTags (Flatten (a > b))))) str, FlatR (Init (Fuse (RenderRecursive (PruneTags (Flatten (a > b)))))) str, FlatR (Last' (Fuse (RenderRecursive (PruneTags (Flatten (a > b)))))) str) => FlatR [a > b] str where
  {-# INLINE flatR #-}
  flatR xs = [mconcat $ concatMap renderList xs]

instance (FlatR (a # b) str, FlatR (Fuse (RenderRecursive (PruneTags (Flatten (a # b))))) str, FlatR (Init (Fuse (RenderRecursive (PruneTags (Flatten (a # b)))))) str, FlatR (Last' (Fuse (RenderRecursive (PruneTags (Flatten (a # b)))))) str) => FlatR [a # b] str where
  {-# INLINE flatR #-}
  flatR xs = [mconcat $ concatMap renderList xs]

newtype Helper (a :: Bool) b = Helper b

class DoRender a b where
  doRender :: IsString b => a -> b

instance KnownSymbol a => DoRender (Proxy a) b where
  {-# INLINE doRender #-}
  doRender = fromString . symbolVal

instance DoRender a b => DoRender (Maybe a) b where
  {-# INLINE doRender #-}
  doRender Nothing = ""
  doRender (Just x) = doRender x

instance DoRender Attribute b where
  {-# INLINE doRender #-}
  doRender (Attribute xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

instance {-# OVERLAPPING #-} DoRender String String where doRender = id
instance DoRender String a where doRender = fromString

instance {-# OVERLAPPING #-} DoRender T.Text T.Text where doRender = id
instance DoRender T.Text a where doRender = fromString . T.unpack

instance {-# OVERLAPPING #-} DoRender LT.Text LT.Text where doRender = id
instance DoRender LT.Text a where doRender = fromString . LT.unpack

instance {-# OVERLAPPING #-} DoRender TLB.Builder TLB.Builder where doRender = id
instance DoRender TLB.Builder a where doRender = fromString . LT.unpack . TLB.toLazyText

instance {-# OVERLAPPING #-} DoRender BS8.ByteString BS8.ByteString where doRender = id
instance DoRender BS8.ByteString a where doRender = fromString . BS8.unpack

instance {-# OVERLAPPING #-} DoRender LBS8.ByteString LBS8.ByteString where doRender = id
instance DoRender LBS8.ByteString a where
  doRender = fromString . LBS8.unpack

instance {-# OVERLAPPABLE #-} Show a => DoRender a b where doRender = fromString . show

instance Render (a # b) String => Show (a # b) where
  show = render

instance Render (a > b) String => Show (a > b) where
  show = render

data EndOfOpen = EndOfOpen
data Open (a :: Element) = Open
data Close (a :: Element) = Close
newtype Attribute = Attribute [(String, String)]

data (#) a b = (:#:) a b
(#) :: a -> b -> a # b
(#) = (:#:)
infixr 5 #

data (>) (a :: Element) b where
  Child :: (a ?> b) => b -> a > b
--  WithAttributes :: (a ?> b) => [(String, String)] -> b -> a > b

infixr 8 >

data ElementInfo
  (contentCategories :: [ContentCategory])
  (permittedContent  :: ContentCategory)
  (tagOmission       :: TagOmission)

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

data TagOmission
  = NoOmission
  | RightOmission
  | LastChildOrFollowedBy [Element]

type family TestPaternity a b c :: Bool where
  TestPaternity a (ElementInfo _ ps _) (ElementInfo cs _ _) = CheckContentCategory ps (a ': cs)

type family CheckContentCategory (a :: ContentCategory) (b :: [ContentCategory]) :: Bool where
  CheckContentCategory (a :|: b) c = CheckContentCategory a c || CheckContentCategory b c
  CheckContentCategory (a :&: b) c = CheckContentCategory a c && CheckContentCategory b c
  CheckContentCategory (NOT a) c = Not (CheckContentCategory a c)
  CheckContentCategory a c = Elem a c

type family (a :: Element) ?> b :: Constraint where
  a ?> (b # c)    = (a ?> b, a ?> c)
  a ?> (b > _)    = MaybeTypeError a b (TestPaternity (SingleElement b) (GetInfo a) (GetInfo b))
  a ?> Maybe b    = a ?> b
  a ?> Either b c = (a ?> b, a ?> c)
  a ?> f (b > c)  = a ?> (b > c)
  a ?> f (b # c)  = a ?> (b # c)
  a ?> ()         = ()
  a ?> b          = CheckString a
--  a ?> b       = TypeError (ShowType b :<>: Text " is not a valid child of " :<>: ShowType a)

type family CheckString (a :: Element) where
  CheckString a = If (TestPaternity OnlyText (GetInfo a) (ElementInfo '[FlowContent, PhrasingContent] NoContent NoOmission))
                   (() :: Constraint)
                   (TypeError (ShowType a :<>: Text " can't contain a string"))

--(?) :: (b ?> c) => (a -> b > c) -> [(String, String)] -> (a -> b > c)
--f ? xs = addAttributes xs . f
--infixr 9 ?

type family Rep n x where
  Rep 0 _ = TypeError (Text "Can't replicate 0 times")
  Rep 1 x = x
  Rep n x = x # Rep (n-1) x

class Replicate n x  where
  replicateH :: Proxy n -> x -> Rep n x

instance {-# OVERLAPPING #-} Replicate 1 x where
  replicateH _ x = x

instance (Replicate (n-1) x, Rep n x ~ (x # Rep (n-1) x)) => Replicate n x where
  replicateH _ x = x # replicateH (Proxy :: Proxy (n-1)) x

-- addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a > b)
-- addAttributes xs (Child b) = WithAttributes xs b
-- addAttributes xs (WithAttributes xs0 b) = WithAttributes (xs0 ++ xs) b

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
  Elem a '[] = False

-- Generated Code:
-- unlines $ map (\x -> map toLower (show x) ++ "_ :: (" ++ show x ++ " ?> a) => a -> " ++ show x ++ " > a\n"++ map toLower (show x) ++ "_ = Child\n" ) [(minBound :: Element) ..]

a_ :: (A ?> a) => a -> A > a
a_ = Child

abbr_ :: (Abbr ?> a) => a -> Abbr > a
abbr_ = Child

acronym_ :: (Acronym ?> a) => a -> Acronym > a
acronym_ = Child

address_ :: (Address ?> a) => a -> Address > a
address_ = Child

applet_ :: (Applet ?> a) => a -> Applet > a
applet_ = Child

area_ :: Area > ()
area_ = Child ()

article_ :: (Article ?> a) => a -> Article > a
article_ = Child

aside_ :: (Aside ?> a) => a -> Aside > a
aside_ = Child

audio_ :: (Audio ?> a) => a -> Audio > a
audio_ = Child

b_ :: (B ?> a) => a -> B > a
b_ = Child

base_ :: Base > ()
base_ = Child ()

basefont_ :: (Basefont ?> a) => a -> Basefont > a
basefont_ = Child

bdi_ :: (Bdi ?> a) => a -> Bdi > a
bdi_ = Child

bdo_ :: (Bdo ?> a) => a -> Bdo > a
bdo_ = Child

bgsound_ :: (Bgsound ?> a) => a -> Bgsound > a
bgsound_ = Child

big_ :: (Big ?> a) => a -> Big > a
big_ = Child

blink_ :: (Blink ?> a) => a -> Blink > a
blink_ = Child

blockquote_ :: (Blockquote ?> a) => a -> Blockquote > a
blockquote_ = Child

body_ :: (Body ?> a) => a -> Body > a
body_ = Child

br_ :: Br > ()
br_ = Child ()

button_ :: (Button ?> a) => a -> Button > a
button_ = Child

canvas_ :: (Canvas ?> a) => a -> Canvas > a
canvas_ = Child

caption_ :: (Caption ?> a) => a -> Caption > a
caption_ = Child

center_ :: (Center ?> a) => a -> Center > a
center_ = Child

cite_ :: (Cite ?> a) => a -> Cite > a
cite_ = Child

code_ :: (Code ?> a) => a -> Code > a
code_ = Child

col_ :: Col > ()
col_ = Child ()

colgroup_ :: (Colgroup ?> a) => a -> Colgroup > a
colgroup_ = Child

command_ :: (Command ?> a) => a -> Command > a
command_ = Child

content_ :: (Content ?> a) => a -> Content > a
content_ = Child

data_ :: (Data ?> a) => a -> Data > a
data_ = Child

datalist_ :: (Datalist ?> a) => a -> Datalist > a
datalist_ = Child

dd_ :: (Dd ?> a) => a -> Dd > a
dd_ = Child

del_ :: (Del ?> a) => a -> Del > a
del_ = Child

details_ :: (Details ?> a) => a -> Details > a
details_ = Child

dfn_ :: (Dfn ?> a) => a -> Dfn > a
dfn_ = Child

dialog_ :: (Dialog ?> a) => a -> Dialog > a
dialog_ = Child

dir_ :: (Dir ?> a) => a -> Dir > a
dir_ = Child

div_ :: (Div ?> a) => a -> Div > a
div_ = Child

dl_ :: (Dl ?> a) => a -> Dl > a
dl_ = Child

dt_ :: (Dt ?> a) => a -> Dt > a
dt_ = Child

element_ :: ('Element ?> a) => a -> 'Element > a
element_ = Child

em_ :: (Em ?> a) => a -> Em > a
em_ = Child

embed_ :: Embed > ()
embed_ = Child ()

fieldset_ :: (Fieldset ?> a) => a -> Fieldset > a
fieldset_ = Child

figcaption_ :: (Figcaption ?> a) => a -> Figcaption > a
figcaption_ = Child

figure_ :: (Figure ?> a) => a -> Figure > a
figure_ = Child

font_ :: (Font ?> a) => a -> Font > a
font_ = Child

footer_ :: (Footer ?> a) => a -> Footer > a
footer_ = Child

form_ :: (Form ?> a) => a -> Form > a
form_ = Child

frame_ :: (Frame ?> a) => a -> Frame > a
frame_ = Child

frameset_ :: (Frameset ?> a) => a -> Frameset > a
frameset_ = Child

h1_ :: (H1 ?> a) => a -> H1 > a
h1_ = Child

h2_ :: (H2 ?> a) => a -> H2 > a
h2_ = Child

h3_ :: (H3 ?> a) => a -> H3 > a
h3_ = Child

h4_ :: (H4 ?> a) => a -> H4 > a
h4_ = Child

h5_ :: (H5 ?> a) => a -> H5 > a
h5_ = Child

h6_ :: (H6 ?> a) => a -> H6 > a
h6_ = Child

head_ :: (Head ?> a) => a -> Head > a
head_ = Child

header_ :: (Header ?> a) => a -> Header > a
header_ = Child

hgroup_ :: (Hgroup ?> a) => a -> Hgroup > a
hgroup_ = Child

hr_ :: Hr > ()
hr_ = Child ()

html_ :: (Html ?> a) => a -> Html > a
html_ = Child

i_ :: (I ?> a) => a -> I > a
i_ = Child

iframe_ :: Iframe > ()
iframe_ = Child ()

image_ :: (Image ?> a) => a -> Image > a
image_ = Child

img_ :: Img > ()
img_ = Child ()

input_ :: (Input ?> a) => a -> Input > a
input_ = Child

ins_ :: (Ins ?> a) => a -> Ins > a
ins_ = Child

isindex_ :: (Isindex ?> a) => a -> Isindex > a
isindex_ = Child

kbd_ :: (Kbd ?> a) => a -> Kbd > a
kbd_ = Child

keygen_ :: (Keygen ?> a) => a -> Keygen > a
keygen_ = Child

label_ :: (Label ?> a) => a -> Label > a
label_ = Child

legend_ :: (Legend ?> a) => a -> Legend > a
legend_ = Child

li_ :: (Li ?> a) => a -> Li > a
li_ = Child

link_ :: Link > ()
link_ = Child ()

listing_ :: (Listing ?> a) => a -> Listing > a
listing_ = Child

main_ :: (Main ?> a) => a -> Main > a
main_ = Child

map_ :: (Map ?> a) => a -> Map > a
map_ = Child

mark_ :: (Mark ?> a) => a -> Mark > a
mark_ = Child

marquee_ :: (Marquee ?> a) => a -> Marquee > a
marquee_ = Child

math_ :: (Math ?> a) => a -> Math > a
math_ = Child

menu_ :: (Menu ?> a) => a -> Menu > a
menu_ = Child

menuitem_ :: Menuitem > ()
menuitem_ = Child ()

meta_ :: Meta > ()
meta_ = Child ()

meter_ :: (Meter ?> a) => a -> Meter > a
meter_ = Child

multicol_ :: (Multicol ?> a) => a -> Multicol > a
multicol_ = Child

nav_ :: (Nav ?> a) => a -> Nav > a
nav_ = Child

nextid_ :: (Nextid ?> a) => a -> Nextid > a
nextid_ = Child

nobr_ :: (Nobr ?> a) => a -> Nobr > a
nobr_ = Child

noembed_ :: (Noembed ?> a) => a -> Noembed > a
noembed_ = Child

noframes_ :: (Noframes ?> a) => a -> Noframes > a
noframes_ = Child

noscript_ :: (Noscript ?> a) => a -> Noscript > a
noscript_ = Child

object_ :: (Object ?> a) => a -> Object > a
object_ = Child

ol_ :: (Ol ?> a) => a -> Ol > a
ol_ = Child

optgroup_ :: (Optgroup ?> a) => a -> Optgroup > a
optgroup_ = Child

option_ :: (Option ?> a) => a -> Option > a
option_ = Child

output_ :: (Output ?> a) => a -> Output > a
output_ = Child

p_ :: (P ?> a) => a -> P > a
p_ = Child

param_ :: Param > ()
param_ = Child ()

picture_ :: (Picture ?> a) => a -> Picture > a
picture_ = Child

plaintext_ :: (Plaintext ?> a) => a -> Plaintext > a
plaintext_ = Child

pre_ :: (Pre ?> a) => a -> Pre > a
pre_ = Child

progress_ :: (Progress ?> a) => a -> Progress > a
progress_ = Child

q_ :: (Q ?> a) => a -> Q > a
q_ = Child

rp_ :: (Rp ?> a) => a -> Rp > a
rp_ = Child

rt_ :: (Rt ?> a) => a -> Rt > a
rt_ = Child

rtc_ :: (Rtc ?> a) => a -> Rtc > a
rtc_ = Child

ruby_ :: (Ruby ?> a) => a -> Ruby > a
ruby_ = Child

s_ :: (S ?> a) => a -> S > a
s_ = Child

samp_ :: (Samp ?> a) => a -> Samp > a
samp_ = Child

script_ :: (Script ?> a) => a -> Script > a
script_ = Child

section_ :: (Section ?> a) => a -> Section > a
section_ = Child

select_ :: (Select ?> a) => a -> Select > a
select_ = Child

shadow_ :: (Shadow ?> a) => a -> Shadow > a
shadow_ = Child

slot_ :: (Slot ?> a) => a -> Slot > a
slot_ = Child

small_ :: (Small ?> a) => a -> Small > a
small_ = Child

source_ :: Source > ()
source_ = Child ()

spacer_ :: (Spacer ?> a) => a -> Spacer > a
spacer_ = Child

span_ :: (Span ?> a) => a -> Span > a
span_ = Child

strike_ :: (Strike ?> a) => a -> Strike > a
strike_ = Child

strong_ :: (Strong ?> a) => a -> Strong > a
strong_ = Child

style_ :: (Style ?> a) => a -> Style > a
style_ = Child

sub_ :: (Sub ?> a) => a -> Sub > a
sub_ = Child

summary_ :: (Summary ?> a) => a -> Summary > a
summary_ = Child

sup_ :: (Sup ?> a) => a -> Sup > a
sup_ = Child

svg_ :: (Svg ?> a) => a -> Svg > a
svg_ = Child

table_ :: (Table ?> a) => a -> Table > a
table_ = Child

tbody_ :: (Tbody ?> a) => a -> Tbody > a
tbody_ = Child

td_ :: (Td ?> a) => a -> Td > a
td_ = Child

template_ :: (Template ?> a) => a -> Template > a
template_ = Child

textarea_ :: (Textarea ?> a) => a -> Textarea > a
textarea_ = Child

tfoot_ :: (Tfoot ?> a) => a -> Tfoot > a
tfoot_ = Child

th_ :: (Th ?> a) => a -> Th > a
th_ = Child

thead_ :: (Thead ?> a) => a -> Thead > a
thead_ = Child

time_ :: (Time ?> a) => a -> Time > a
time_ = Child

title_ :: (Title ?> a) => a -> Title > a
title_ = Child

tr_ :: (Tr ?> a) => a -> Tr > a
tr_ = Child

track_ :: Track > ()
track_ = Child ()

tt_ :: (Tt ?> a) => a -> Tt > a
tt_ = Child

u_ :: (U ?> a) => a -> U > a
u_ = Child

ul_ :: (Ul ?> a) => a -> Ul > a
ul_ = Child

var_ :: (Var ?> a) => a -> Var > a
var_ = Child

video_ :: (Video ?> a) => a -> Video > a
video_ = Child

wbr_ :: Wbr > ()
wbr_ = Child ()

xmp_ :: (Xmp ?> a) => a -> Xmp > a
xmp_ = Child
