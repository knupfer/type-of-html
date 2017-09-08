{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MagicHash         #-}

module Html.Attribute where

import Html.Convert
import Html.Type
import Data.Semigroup
import Data.ByteString.Builder
import GHC.Prim (Addr#)

import qualified Data.ByteString.Builder.Internal as U
import qualified Data.ByteString.Internal as U
import qualified Data.ByteString.Unsafe   as U

{-# INLINE unsafe #-}
unsafe :: Int -> Addr# -> U.ByteString
unsafe i addr = U.accursedUnutterablePerformIO (U.unsafePackAddressLen i addr)

{-# INLINE addAttributes #-}
addAttributes :: (a ?> b) => Attribute -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes xs b

{-# INLINE accept_ #-}
accept_ :: Convert a => a -> Attribute
accept_ x = Attribute $ U.byteStringCopy (unsafe 9 " accept=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE acceptcharset_ #-}
acceptcharset_ :: Convert a => a -> Attribute
acceptcharset_ x = Attribute $ U.byteStringCopy (unsafe 16 " acceptcharset=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE accesskey_ #-}
accesskey_ :: Convert a => a -> Attribute
accesskey_ x = Attribute $ U.byteStringCopy (unsafe 12 " accesskey=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE action_ #-}
action_ :: Convert a => a -> Attribute
action_ x = Attribute $ U.byteStringCopy (unsafe 9 " action=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE align_ #-}
align_ :: Convert a => a -> Attribute
align_ x = Attribute $ U.byteStringCopy (unsafe 8 " align=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE alt_ #-}
alt_ :: Convert a => a -> Attribute
alt_ x = Attribute $ U.byteStringCopy (unsafe 6 " alt=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE async_ #-}
async_ :: Convert a => a -> Attribute
async_ x = Attribute $ U.byteStringCopy (unsafe 8 " async=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE autocomplete_ #-}
autocomplete_ :: Convert a => a -> Attribute
autocomplete_ x = Attribute $ U.byteStringCopy (unsafe 15 " autocomplete=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE autofocus_ #-}
autofocus_ :: Convert a => a -> Attribute
autofocus_ x = Attribute $ U.byteStringCopy (unsafe 12 " autofocus=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE autoplay_ #-}
autoplay_ :: Convert a => a -> Attribute
autoplay_ x = Attribute $ U.byteStringCopy (unsafe 11 " autoplay=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE autosave_ #-}
autosave_ :: Convert a => a -> Attribute
autosave_ x = Attribute $ U.byteStringCopy (unsafe 11 " autosave=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE bgcolor_ #-}
bgcolor_ :: Convert a => a -> Attribute
bgcolor_ x = Attribute $ U.byteStringCopy (unsafe 10 " bgcolor=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE border_ #-}
border_ :: Convert a => a -> Attribute
border_ x = Attribute $ U.byteStringCopy (unsafe 9 " border=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE buffered_ #-}
buffered_ :: Convert a => a -> Attribute
buffered_ x = Attribute $ U.byteStringCopy (unsafe 11 " buffered=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE challenge_ #-}
challenge_ :: Convert a => a -> Attribute
challenge_ x = Attribute $ U.byteStringCopy (unsafe 12 " challenge=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE charset_ #-}
charset_ :: Convert a => a -> Attribute
charset_ x = Attribute $ U.byteStringCopy (unsafe 10 " charset=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE checked_ #-}
checked_ :: Convert a => a -> Attribute
checked_ x = Attribute $ U.byteStringCopy (unsafe 10 " checked=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE cite_ #-}
cite_ :: Convert a => a -> Attribute
cite_ x = Attribute $ U.byteStringCopy (unsafe 7 " cite=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE class_ #-}
class_ :: Convert a => a -> Attribute
class_ x = Attribute $ U.byteStringCopy (unsafe 8 " class=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE code_ #-}
code_ :: Convert a => a -> Attribute
code_ x = Attribute $ U.byteStringCopy (unsafe 7 " code=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE codebase_ #-}
codebase_ :: Convert a => a -> Attribute
codebase_ x = Attribute $ U.byteStringCopy (unsafe 11 " codebase=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE color_ #-}
color_ :: Convert a => a -> Attribute
color_ x = Attribute $ U.byteStringCopy (unsafe 8 " color=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE cols_ #-}
cols_ :: Convert a => a -> Attribute
cols_ x = Attribute $ U.byteStringCopy (unsafe 7 " cols=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE colspan_ #-}
colspan_ :: Convert a => a -> Attribute
colspan_ x = Attribute $ U.byteStringCopy (unsafe 10 " colspan=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE content_ #-}
content_ :: Convert a => a -> Attribute
content_ x = Attribute $ U.byteStringCopy (unsafe 10 " content=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE contenteditable_ #-}
contenteditable_ :: Convert a => a -> Attribute
contenteditable_ x = Attribute $ U.byteStringCopy (unsafe 18 " contenteditable=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE contextmenu_ #-}
contextmenu_ :: Convert a => a -> Attribute
contextmenu_ x = Attribute $ U.byteStringCopy (unsafe 14 " contextmenu=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE controls_ #-}
controls_ :: Convert a => a -> Attribute
controls_ x = Attribute $ U.byteStringCopy (unsafe 11 " controls=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE coords_ #-}
coords_ :: Convert a => a -> Attribute
coords_ x = Attribute $ U.byteStringCopy (unsafe 9 " coords=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE crossorigin_ #-}
crossorigin_ :: Convert a => a -> Attribute
crossorigin_ x = Attribute $ U.byteStringCopy (unsafe 14 " crossorigin=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE data_ #-}
data_ :: Convert a => a -> Attribute
data_ x = Attribute $ U.byteStringCopy (unsafe 7 " data=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE datetime_ #-}
datetime_ :: Convert a => a -> Attribute
datetime_ x = Attribute $ U.byteStringCopy (unsafe 11 " datetime=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE default_ #-}
default_ :: Convert a => a -> Attribute
default_ x = Attribute $ U.byteStringCopy (unsafe 10 " default=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE defer_ #-}
defer_ :: Convert a => a -> Attribute
defer_ x = Attribute $ U.byteStringCopy (unsafe 8 " defer=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE dir_ #-}
dir_ :: Convert a => a -> Attribute
dir_ x = Attribute $ U.byteStringCopy (unsafe 6 " dir=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE dirname_ #-}
dirname_ :: Convert a => a -> Attribute
dirname_ x = Attribute $ U.byteStringCopy (unsafe 10 " dirname=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE disabled_ #-}
disabled_ :: Convert a => a -> Attribute
disabled_ x = Attribute $ U.byteStringCopy (unsafe 11 " disabled=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE download_ #-}
download_ :: Convert a => a -> Attribute
download_ x = Attribute $ U.byteStringCopy (unsafe 11 " download=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE draggable_ #-}
draggable_ :: Convert a => a -> Attribute
draggable_ x = Attribute $ U.byteStringCopy (unsafe 12 " draggable=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE dropzone_ #-}
dropzone_ :: Convert a => a -> Attribute
dropzone_ x = Attribute $ U.byteStringCopy (unsafe 11 " dropzone=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE enctype_ #-}
enctype_ :: Convert a => a -> Attribute
enctype_ x = Attribute $ U.byteStringCopy (unsafe 10 " enctype=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE for_ #-}
for_ :: Convert a => a -> Attribute
for_ x = Attribute $ U.byteStringCopy (unsafe 6 " for=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE form_ #-}
form_ :: Convert a => a -> Attribute
form_ x = Attribute $ U.byteStringCopy (unsafe 7 " form=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE formaction_ #-}
formaction_ :: Convert a => a -> Attribute
formaction_ x = Attribute $ U.byteStringCopy (unsafe 13 " formaction=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE headers_ #-}
headers_ :: Convert a => a -> Attribute
headers_ x = Attribute $ U.byteStringCopy (unsafe 10 " headers=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE height_ #-}
height_ :: Convert a => a -> Attribute
height_ x = Attribute $ U.byteStringCopy (unsafe 9 " height=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE hidden_ #-}
hidden_ :: Convert a => a -> Attribute
hidden_ x = Attribute $ U.byteStringCopy (unsafe 9 " hidden=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE high_ #-}
high_ :: Convert a => a -> Attribute
high_ x = Attribute $ U.byteStringCopy (unsafe 7 " high=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE href_ #-}
href_ :: Convert a => a -> Attribute
href_ x = Attribute $ U.byteStringCopy (unsafe 7 " href=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE hreflang_ #-}
hreflang_ :: Convert a => a -> Attribute
hreflang_ x = Attribute $ U.byteStringCopy (unsafe 11 " hreflang=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE httpequiv_ #-}
httpequiv_ :: Convert a => a -> Attribute
httpequiv_ x = Attribute $ U.byteStringCopy (unsafe 12 " httpequiv=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE icon_ #-}
icon_ :: Convert a => a -> Attribute
icon_ x = Attribute $ U.byteStringCopy (unsafe 7 " icon=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE id_ #-}
id_ :: Convert a => a -> Attribute
id_ x = Attribute $ U.byteStringCopy (unsafe 5 " id=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE integrity_ #-}
integrity_ :: Convert a => a -> Attribute
integrity_ x = Attribute $ U.byteStringCopy (unsafe 12 " integrity=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE ismap_ #-}
ismap_ :: Convert a => a -> Attribute
ismap_ x = Attribute $ U.byteStringCopy (unsafe 8 " ismap=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE itemprop_ #-}
itemprop_ :: Convert a => a -> Attribute
itemprop_ x = Attribute $ U.byteStringCopy (unsafe 11 " itemprop=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE keytype_ #-}
keytype_ :: Convert a => a -> Attribute
keytype_ x = Attribute $ U.byteStringCopy (unsafe 10 " keytype=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE kind_ #-}
kind_ :: Convert a => a -> Attribute
kind_ x = Attribute $ U.byteStringCopy (unsafe 7 " kind=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE label_ #-}
label_ :: Convert a => a -> Attribute
label_ x = Attribute $ U.byteStringCopy (unsafe 8 " label=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE lang_ #-}
lang_ :: Convert a => a -> Attribute
lang_ x = Attribute $ U.byteStringCopy (unsafe 7 " lang=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE language_ #-}
language_ :: Convert a => a -> Attribute
language_ x = Attribute $ U.byteStringCopy (unsafe 11 " language=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE list_ #-}
list_ :: Convert a => a -> Attribute
list_ x = Attribute $ U.byteStringCopy (unsafe 7 " list=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE loop_ #-}
loop_ :: Convert a => a -> Attribute
loop_ x = Attribute $ U.byteStringCopy (unsafe 7 " loop=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE low_ #-}
low_ :: Convert a => a -> Attribute
low_ x = Attribute $ U.byteStringCopy (unsafe 6 " low=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE manifest_ #-}
manifest_ :: Convert a => a -> Attribute
manifest_ x = Attribute $ U.byteStringCopy (unsafe 11 " manifest=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE max_ #-}
max_ :: Convert a => a -> Attribute
max_ x = Attribute $ U.byteStringCopy (unsafe 6 " max=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE maxlength_ #-}
maxlength_ :: Convert a => a -> Attribute
maxlength_ x = Attribute $ U.byteStringCopy (unsafe 12 " maxlength=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE minlength_ #-}
minlength_ :: Convert a => a -> Attribute
minlength_ x = Attribute $ U.byteStringCopy (unsafe 6 " minlength=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE media_ #-}
media_ :: Convert a => a -> Attribute
media_ x = Attribute $ U.byteStringCopy (unsafe 8 " media=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE method_ #-}
method_ :: Convert a => a -> Attribute
method_ x = Attribute $ U.byteStringCopy (unsafe 9 " method=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE min_ #-}
min_ :: Convert a => a -> Attribute
min_ x = Attribute $ U.byteStringCopy (unsafe 6 " min=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE multiple_ #-}
multiple_ :: Convert a => a -> Attribute
multiple_ x = Attribute $ U.byteStringCopy (unsafe 11 " multiple=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE muted_ #-}
muted_ :: Convert a => a -> Attribute
muted_ x = Attribute $ U.byteStringCopy (unsafe 8 " muted=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE name_ #-}
name_ :: Convert a => a -> Attribute
name_ x = Attribute $ U.byteStringCopy (unsafe 7 " name=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE novalidate_ #-}
novalidate_ :: Convert a => a -> Attribute
novalidate_ x = Attribute $ U.byteStringCopy (unsafe 13 " novalidate=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE open_ #-}
open_ :: Convert a => a -> Attribute
open_ x = Attribute $ U.byteStringCopy (unsafe 7 " open=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE optimum_ #-}
optimum_ :: Convert a => a -> Attribute
optimum_ x = Attribute $ U.byteStringCopy (unsafe 10 " optimum=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE pattern_ #-}
pattern_ :: Convert a => a -> Attribute
pattern_ x = Attribute $ U.byteStringCopy (unsafe 10 " pattern=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE ping_ #-}
ping_ :: Convert a => a -> Attribute
ping_ x = Attribute $ U.byteStringCopy (unsafe 7 " ping=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE placeholder_ #-}
placeholder_ :: Convert a => a -> Attribute
placeholder_ x = Attribute $ U.byteStringCopy (unsafe 14 " placeholder=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE poster_ #-}
poster_ :: Convert a => a -> Attribute
poster_ x = Attribute $ U.byteStringCopy (unsafe 9 " poster=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE preload_ #-}
preload_ :: Convert a => a -> Attribute
preload_ x = Attribute $ U.byteStringCopy (unsafe 10 " preload=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE radiogroup_ #-}
radiogroup_ :: Convert a => a -> Attribute
radiogroup_ x = Attribute $ U.byteStringCopy (unsafe 13 " radiogroup=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE readonly_ #-}
readonly_ :: Convert a => a -> Attribute
readonly_ x = Attribute $ U.byteStringCopy (unsafe 11 " readonly=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE rel_ #-}
rel_ :: Convert a => a -> Attribute
rel_ x = Attribute $ U.byteStringCopy (unsafe 6 " rel=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE required_ #-}
required_ :: Convert a => a -> Attribute
required_ x = Attribute $ U.byteStringCopy (unsafe 11 " required=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE reversed_ #-}
reversed_ :: Convert a => a -> Attribute
reversed_ x = Attribute $ U.byteStringCopy (unsafe 11 " reversed=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE rows_ #-}
rows_ :: Convert a => a -> Attribute
rows_ x = Attribute $ U.byteStringCopy (unsafe 7 " rows=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE rowspan_ #-}
rowspan_ :: Convert a => a -> Attribute
rowspan_ x = Attribute $ U.byteStringCopy (unsafe 10 " rowspan=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE sandbox_ #-}
sandbox_ :: Convert a => a -> Attribute
sandbox_ x = Attribute $ U.byteStringCopy (unsafe 10 " sandbox=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE scope_ #-}
scope_ :: Convert a => a -> Attribute
scope_ x = Attribute $ U.byteStringCopy (unsafe 8 " scope=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE scoped_ #-}
scoped_ :: Convert a => a -> Attribute
scoped_ x = Attribute $ U.byteStringCopy (unsafe 9 " scoped=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE seamless_ #-}
seamless_ :: Convert a => a -> Attribute
seamless_ x = Attribute $ U.byteStringCopy (unsafe 11 " seamless=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE selected_ #-}
selected_ :: Convert a => a -> Attribute
selected_ x = Attribute $ U.byteStringCopy (unsafe 11 " selected=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE shape_ #-}
shape_ :: Convert a => a -> Attribute
shape_ x = Attribute $ U.byteStringCopy (unsafe 8 " shape=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE size_ #-}
size_ :: Convert a => a -> Attribute
size_ x = Attribute $ U.byteStringCopy (unsafe 7 " size=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE sizes_ #-}
sizes_ :: Convert a => a -> Attribute
sizes_ x = Attribute $ U.byteStringCopy (unsafe 8 " sizes=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE slot_ #-}
slot_ :: Convert a => a -> Attribute
slot_ x = Attribute $ U.byteStringCopy (unsafe 7 " slot=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE span_ #-}
span_ :: Convert a => a -> Attribute
span_ x = Attribute $ U.byteStringCopy (unsafe 7 " span=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE spellcheck_ #-}
spellcheck_ :: Convert a => a -> Attribute
spellcheck_ x = Attribute $ U.byteStringCopy (unsafe 13 " spellcheck=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE src_ #-}
src_ :: Convert a => a -> Attribute
src_ x = Attribute $ U.byteStringCopy (unsafe 6 " src=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE srcdoc_ #-}
srcdoc_ :: Convert a => a -> Attribute
srcdoc_ x = Attribute $ U.byteStringCopy (unsafe 9 " srcdoc=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE srclang_ #-}
srclang_ :: Convert a => a -> Attribute
srclang_ x = Attribute $ U.byteStringCopy (unsafe 10 " srclang=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE srcset_ #-}
srcset_ :: Convert a => a -> Attribute
srcset_ x = Attribute $ U.byteStringCopy (unsafe 9 " srcset=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE start_ #-}
start_ :: Convert a => a -> Attribute
start_ x = Attribute $ U.byteStringCopy (unsafe 8 " start=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE step_ #-}
step_ :: Convert a => a -> Attribute
step_ x = Attribute $ U.byteStringCopy (unsafe 7 " step=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE style_ #-}
style_ :: Convert a => a -> Attribute
style_ x = Attribute $ U.byteStringCopy (unsafe 8 " style=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE summary_ #-}
summary_ :: Convert a => a -> Attribute
summary_ x = Attribute $ U.byteStringCopy (unsafe 10 " summary=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE tabindex_ #-}
tabindex_ :: Convert a => a -> Attribute
tabindex_ x = Attribute $ U.byteStringCopy (unsafe 11 " tabindex=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE target_ #-}
target_ :: Convert a => a -> Attribute
target_ x = Attribute $ U.byteStringCopy (unsafe 9 " target=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE title_ #-}
title_ :: Convert a => a -> Attribute
title_ x = Attribute $ U.byteStringCopy (unsafe 8 " title=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE type_ #-}
type_ :: Convert a => a -> Attribute
type_ x = Attribute $ U.byteStringCopy (unsafe 7 " type=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE usemap_ #-}
usemap_ :: Convert a => a -> Attribute
usemap_ x = Attribute $ U.byteStringCopy (unsafe 9 " usemap=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE value_ #-}
value_ :: Convert a => a -> Attribute
value_ x = Attribute $ U.byteStringCopy (unsafe 8 " value=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE width_ #-}
width_ :: Convert a => a -> Attribute
width_ x = Attribute $ U.byteStringCopy (unsafe 8 " width=\""#) <> unConv (convert x) <> char7 '"'

{-# INLINE wrap_ #-}
wrap_ :: Convert a => a -> Attribute
wrap_ x = Attribute $ U.byteStringCopy (unsafe 7 " wrap=\""#) <> unConv (convert x) <> char7 '"'
