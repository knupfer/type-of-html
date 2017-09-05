{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Html.Attribute where

import Html.Convert
import Html.Type
import Data.Semigroup

{-# INLINE addAttributes #-}
addAttributes :: (a ?> b) => [Attribute] -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes xs b

{-# INLINE accept_ #-}
accept_ :: Convert a => a -> Attribute
accept_ x = Attribute $ " accept=\"" <> unConv (conv x) <> "\""

{-# INLINE acceptcharset_ #-}
acceptcharset_ :: Convert a => a -> Attribute
acceptcharset_ x = Attribute $ " acceptcharset=\"" <> unConv (conv x) <> "\""

{-# INLINE accesskey_ #-}
accesskey_ :: Convert a => a -> Attribute
accesskey_ x = Attribute $ " accesskey=\"" <> unConv (conv x) <> "\""

{-# INLINE action_ #-}
action_ :: Convert a => a -> Attribute
action_ x = Attribute $ " action=\"" <> unConv (conv x) <> "\""

{-# INLINE align_ #-}
align_ :: Convert a => a -> Attribute
align_ x = Attribute $ " align=\"" <> unConv (conv x) <> "\""

{-# INLINE alt_ #-}
alt_ :: Convert a => a -> Attribute
alt_ x = Attribute $ " alt=\"" <> unConv (conv x) <> "\""

{-# INLINE async_ #-}
async_ :: Convert a => a -> Attribute
async_ x = Attribute $ " async=\"" <> unConv (conv x) <> "\""

{-# INLINE autocomplete_ #-}
autocomplete_ :: Convert a => a -> Attribute
autocomplete_ x = Attribute $ " autocomplete=\"" <> unConv (conv x) <> "\""

{-# INLINE autofocus_ #-}
autofocus_ :: Convert a => a -> Attribute
autofocus_ x = Attribute $ " autofocus=\"" <> unConv (conv x) <> "\""

{-# INLINE autoplay_ #-}
autoplay_ :: Convert a => a -> Attribute
autoplay_ x = Attribute $ " autoplay=\"" <> unConv (conv x) <> "\""

{-# INLINE autosave_ #-}
autosave_ :: Convert a => a -> Attribute
autosave_ x = Attribute $ " autosave=\"" <> unConv (conv x) <> "\""

{-# INLINE bgcolor_ #-}
bgcolor_ :: Convert a => a -> Attribute
bgcolor_ x = Attribute $ " bgcolor=\"" <> unConv (conv x) <> "\""

{-# INLINE border_ #-}
border_ :: Convert a => a -> Attribute
border_ x = Attribute $ " border=\"" <> unConv (conv x) <> "\""

{-# INLINE buffered_ #-}
buffered_ :: Convert a => a -> Attribute
buffered_ x = Attribute $ " buffered=\"" <> unConv (conv x) <> "\""

{-# INLINE challenge_ #-}
challenge_ :: Convert a => a -> Attribute
challenge_ x = Attribute $ " challenge=\"" <> unConv (conv x) <> "\""

{-# INLINE charset_ #-}
charset_ :: Convert a => a -> Attribute
charset_ x = Attribute $ " charset=\"" <> unConv (conv x) <> "\""

{-# INLINE checked_ #-}
checked_ :: Convert a => a -> Attribute
checked_ x = Attribute $ " checked=\"" <> unConv (conv x) <> "\""

{-# INLINE cite_ #-}
cite_ :: Convert a => a -> Attribute
cite_ x = Attribute $ " cite=\"" <> unConv (conv x) <> "\""

{-# INLINE class_ #-}
class_ :: Convert a => a -> Attribute
class_ x = Attribute $ " class=\"" <> unConv (conv x) <> "\""

{-# INLINE code_ #-}
code_ :: Convert a => a -> Attribute
code_ x = Attribute $ " code=\"" <> unConv (conv x) <> "\""

{-# INLINE codebase_ #-}
codebase_ :: Convert a => a -> Attribute
codebase_ x = Attribute $ " codebase=\"" <> unConv (conv x) <> "\""

{-# INLINE color_ #-}
color_ :: Convert a => a -> Attribute
color_ x = Attribute $ " color=\"" <> unConv (conv x) <> "\""

{-# INLINE cols_ #-}
cols_ :: Convert a => a -> Attribute
cols_ x = Attribute $ " cols=\"" <> unConv (conv x) <> "\""

{-# INLINE colspan_ #-}
colspan_ :: Convert a => a -> Attribute
colspan_ x = Attribute $ " colspan=\"" <> unConv (conv x) <> "\""

{-# INLINE content_ #-}
content_ :: Convert a => a -> Attribute
content_ x = Attribute $ " content=\"" <> unConv (conv x) <> "\""

{-# INLINE contenteditable_ #-}
contenteditable_ :: Convert a => a -> Attribute
contenteditable_ x = Attribute $ " contenteditable=\"" <> unConv (conv x) <> "\""

{-# INLINE contextmenu_ #-}
contextmenu_ :: Convert a => a -> Attribute
contextmenu_ x = Attribute $ " contextmenu=\"" <> unConv (conv x) <> "\""

{-# INLINE controls_ #-}
controls_ :: Convert a => a -> Attribute
controls_ x = Attribute $ " controls=\"" <> unConv (conv x) <> "\""

{-# INLINE coords_ #-}
coords_ :: Convert a => a -> Attribute
coords_ x = Attribute $ " coords=\"" <> unConv (conv x) <> "\""

{-# INLINE crossorigin_ #-}
crossorigin_ :: Convert a => a -> Attribute
crossorigin_ x = Attribute $ " crossorigin=\"" <> unConv (conv x) <> "\""

{-# INLINE data_ #-}
data_ :: Convert a => a -> Attribute
data_ x = Attribute $ " data=\"" <> unConv (conv x) <> "\""

{-# INLINE datetime_ #-}
datetime_ :: Convert a => a -> Attribute
datetime_ x = Attribute $ " datetime=\"" <> unConv (conv x) <> "\""

{-# INLINE default_ #-}
default_ :: Convert a => a -> Attribute
default_ x = Attribute $ " default=\"" <> unConv (conv x) <> "\""

{-# INLINE defer_ #-}
defer_ :: Convert a => a -> Attribute
defer_ x = Attribute $ " defer=\"" <> unConv (conv x) <> "\""

{-# INLINE dir_ #-}
dir_ :: Convert a => a -> Attribute
dir_ x = Attribute $ " dir=\"" <> unConv (conv x) <> "\""

{-# INLINE dirname_ #-}
dirname_ :: Convert a => a -> Attribute
dirname_ x = Attribute $ " dirname=\"" <> unConv (conv x) <> "\""

{-# INLINE disabled_ #-}
disabled_ :: Convert a => a -> Attribute
disabled_ x = Attribute $ " disabled=\"" <> unConv (conv x) <> "\""

{-# INLINE download_ #-}
download_ :: Convert a => a -> Attribute
download_ x = Attribute $ " download=\"" <> unConv (conv x) <> "\""

{-# INLINE draggable_ #-}
draggable_ :: Convert a => a -> Attribute
draggable_ x = Attribute $ " draggable=\"" <> unConv (conv x) <> "\""

{-# INLINE dropzone_ #-}
dropzone_ :: Convert a => a -> Attribute
dropzone_ x = Attribute $ " dropzone=\"" <> unConv (conv x) <> "\""

{-# INLINE enctype_ #-}
enctype_ :: Convert a => a -> Attribute
enctype_ x = Attribute $ " enctype=\"" <> unConv (conv x) <> "\""

{-# INLINE for_ #-}
for_ :: Convert a => a -> Attribute
for_ x = Attribute $ " for=\"" <> unConv (conv x) <> "\""

{-# INLINE form_ #-}
form_ :: Convert a => a -> Attribute
form_ x = Attribute $ " form=\"" <> unConv (conv x) <> "\""

{-# INLINE formaction_ #-}
formaction_ :: Convert a => a -> Attribute
formaction_ x = Attribute $ " formaction=\"" <> unConv (conv x) <> "\""

{-# INLINE headers_ #-}
headers_ :: Convert a => a -> Attribute
headers_ x = Attribute $ " headers=\"" <> unConv (conv x) <> "\""

{-# INLINE height_ #-}
height_ :: Convert a => a -> Attribute
height_ x = Attribute $ " height=\"" <> unConv (conv x) <> "\""

{-# INLINE hidden_ #-}
hidden_ :: Convert a => a -> Attribute
hidden_ x = Attribute $ " hidden=\"" <> unConv (conv x) <> "\""

{-# INLINE high_ #-}
high_ :: Convert a => a -> Attribute
high_ x = Attribute $ " high=\"" <> unConv (conv x) <> "\""

{-# INLINE href_ #-}
href_ :: Convert a => a -> Attribute
href_ x = Attribute $ " href=\"" <> unConv (conv x) <> "\""

{-# INLINE hreflang_ #-}
hreflang_ :: Convert a => a -> Attribute
hreflang_ x = Attribute $ " hreflang=\"" <> unConv (conv x) <> "\""

{-# INLINE httpequiv_ #-}
httpequiv_ :: Convert a => a -> Attribute
httpequiv_ x = Attribute $ " httpequiv=\"" <> unConv (conv x) <> "\""

{-# INLINE icon_ #-}
icon_ :: Convert a => a -> Attribute
icon_ x = Attribute $ " icon=\"" <> unConv (conv x) <> "\""

{-# INLINE id_ #-}
id_ :: Convert a => a -> Attribute
id_ x = Attribute $ " id=\"" <> unConv (conv x) <> "\""

{-# INLINE integrity_ #-}
integrity_ :: Convert a => a -> Attribute
integrity_ x = Attribute $ " integrity=\"" <> unConv (conv x) <> "\""

{-# INLINE ismap_ #-}
ismap_ :: Convert a => a -> Attribute
ismap_ x = Attribute $ " ismap=\"" <> unConv (conv x) <> "\""

{-# INLINE itemprop_ #-}
itemprop_ :: Convert a => a -> Attribute
itemprop_ x = Attribute $ " itemprop=\"" <> unConv (conv x) <> "\""

{-# INLINE keytype_ #-}
keytype_ :: Convert a => a -> Attribute
keytype_ x = Attribute $ " keytype=\"" <> unConv (conv x) <> "\""

{-# INLINE kind_ #-}
kind_ :: Convert a => a -> Attribute
kind_ x = Attribute $ " kind=\"" <> unConv (conv x) <> "\""

{-# INLINE label_ #-}
label_ :: Convert a => a -> Attribute
label_ x = Attribute $ " label=\"" <> unConv (conv x) <> "\""

{-# INLINE lang_ #-}
lang_ :: Convert a => a -> Attribute
lang_ x = Attribute $ " lang=\"" <> unConv (conv x) <> "\""

{-# INLINE language_ #-}
language_ :: Convert a => a -> Attribute
language_ x = Attribute $ " language=\"" <> unConv (conv x) <> "\""

{-# INLINE list_ #-}
list_ :: Convert a => a -> Attribute
list_ x = Attribute $ " list=\"" <> unConv (conv x) <> "\""

{-# INLINE loop_ #-}
loop_ :: Convert a => a -> Attribute
loop_ x = Attribute $ " loop=\"" <> unConv (conv x) <> "\""

{-# INLINE low_ #-}
low_ :: Convert a => a -> Attribute
low_ x = Attribute $ " low=\"" <> unConv (conv x) <> "\""

{-# INLINE manifest_ #-}
manifest_ :: Convert a => a -> Attribute
manifest_ x = Attribute $ " manifest=\"" <> unConv (conv x) <> "\""

{-# INLINE max_ #-}
max_ :: Convert a => a -> Attribute
max_ x = Attribute $ " max=\"" <> unConv (conv x) <> "\""

{-# INLINE maxlength_ #-}
maxlength_ :: Convert a => a -> Attribute
maxlength_ x = Attribute $ " maxlength=\"" <> unConv (conv x) <> "\""

{-# INLINE minlength_ #-}
minlength_ :: Convert a => a -> Attribute
minlength_ x = Attribute $ " minlength=\"" <> unConv (conv x) <> "\""

{-# INLINE media_ #-}
media_ :: Convert a => a -> Attribute
media_ x = Attribute $ " media=\"" <> unConv (conv x) <> "\""

{-# INLINE method_ #-}
method_ :: Convert a => a -> Attribute
method_ x = Attribute $ " method=\"" <> unConv (conv x) <> "\""

{-# INLINE min_ #-}
min_ :: Convert a => a -> Attribute
min_ x = Attribute $ " min=\"" <> unConv (conv x) <> "\""

{-# INLINE multiple_ #-}
multiple_ :: Convert a => a -> Attribute
multiple_ x = Attribute $ " multiple=\"" <> unConv (conv x) <> "\""

{-# INLINE muted_ #-}
muted_ :: Convert a => a -> Attribute
muted_ x = Attribute $ " muted=\"" <> unConv (conv x) <> "\""

{-# INLINE name_ #-}
name_ :: Convert a => a -> Attribute
name_ x = Attribute $ " name=\"" <> unConv (conv x) <> "\""

{-# INLINE novalidate_ #-}
novalidate_ :: Convert a => a -> Attribute
novalidate_ x = Attribute $ " novalidate=\"" <> unConv (conv x) <> "\""

{-# INLINE open_ #-}
open_ :: Convert a => a -> Attribute
open_ x = Attribute $ " open=\"" <> unConv (conv x) <> "\""

{-# INLINE optimum_ #-}
optimum_ :: Convert a => a -> Attribute
optimum_ x = Attribute $ " optimum=\"" <> unConv (conv x) <> "\""

{-# INLINE pattern_ #-}
pattern_ :: Convert a => a -> Attribute
pattern_ x = Attribute $ " pattern=\"" <> unConv (conv x) <> "\""

{-# INLINE ping_ #-}
ping_ :: Convert a => a -> Attribute
ping_ x = Attribute $ " ping=\"" <> unConv (conv x) <> "\""

{-# INLINE placeholder_ #-}
placeholder_ :: Convert a => a -> Attribute
placeholder_ x = Attribute $ " placeholder=\"" <> unConv (conv x) <> "\""

{-# INLINE poster_ #-}
poster_ :: Convert a => a -> Attribute
poster_ x = Attribute $ " poster=\"" <> unConv (conv x) <> "\""

{-# INLINE preload_ #-}
preload_ :: Convert a => a -> Attribute
preload_ x = Attribute $ " preload=\"" <> unConv (conv x) <> "\""

{-# INLINE radiogroup_ #-}
radiogroup_ :: Convert a => a -> Attribute
radiogroup_ x = Attribute $ " radiogroup=\"" <> unConv (conv x) <> "\""

{-# INLINE readonly_ #-}
readonly_ :: Convert a => a -> Attribute
readonly_ x = Attribute $ " readonly=\"" <> unConv (conv x) <> "\""

{-# INLINE rel_ #-}
rel_ :: Convert a => a -> Attribute
rel_ x = Attribute $ " rel=\"" <> unConv (conv x) <> "\""

{-# INLINE required_ #-}
required_ :: Convert a => a -> Attribute
required_ x = Attribute $ " required=\"" <> unConv (conv x) <> "\""

{-# INLINE reversed_ #-}
reversed_ :: Convert a => a -> Attribute
reversed_ x = Attribute $ " reversed=\"" <> unConv (conv x) <> "\""

{-# INLINE rows_ #-}
rows_ :: Convert a => a -> Attribute
rows_ x = Attribute $ " rows=\"" <> unConv (conv x) <> "\""

{-# INLINE rowspan_ #-}
rowspan_ :: Convert a => a -> Attribute
rowspan_ x = Attribute $ " rowspan=\"" <> unConv (conv x) <> "\""

{-# INLINE sandbox_ #-}
sandbox_ :: Convert a => a -> Attribute
sandbox_ x = Attribute $ " sandbox=\"" <> unConv (conv x) <> "\""

{-# INLINE scope_ #-}
scope_ :: Convert a => a -> Attribute
scope_ x = Attribute $ " scope=\"" <> unConv (conv x) <> "\""

{-# INLINE scoped_ #-}
scoped_ :: Convert a => a -> Attribute
scoped_ x = Attribute $ " scoped=\"" <> unConv (conv x) <> "\""

{-# INLINE seamless_ #-}
seamless_ :: Convert a => a -> Attribute
seamless_ x = Attribute $ " seamless=\"" <> unConv (conv x) <> "\""

{-# INLINE selected_ #-}
selected_ :: Convert a => a -> Attribute
selected_ x = Attribute $ " selected=\"" <> unConv (conv x) <> "\""

{-# INLINE shape_ #-}
shape_ :: Convert a => a -> Attribute
shape_ x = Attribute $ " shape=\"" <> unConv (conv x) <> "\""

{-# INLINE size_ #-}
size_ :: Convert a => a -> Attribute
size_ x = Attribute $ " size=\"" <> unConv (conv x) <> "\""

{-# INLINE sizes_ #-}
sizes_ :: Convert a => a -> Attribute
sizes_ x = Attribute $ " sizes=\"" <> unConv (conv x) <> "\""

{-# INLINE slot_ #-}
slot_ :: Convert a => a -> Attribute
slot_ x = Attribute $ " slot=\"" <> unConv (conv x) <> "\""

{-# INLINE span_ #-}
span_ :: Convert a => a -> Attribute
span_ x = Attribute $ " span=\"" <> unConv (conv x) <> "\""

{-# INLINE spellcheck_ #-}
spellcheck_ :: Convert a => a -> Attribute
spellcheck_ x = Attribute $ " spellcheck=\"" <> unConv (conv x) <> "\""

{-# INLINE src_ #-}
src_ :: Convert a => a -> Attribute
src_ x = Attribute $ " src=\"" <> unConv (conv x) <> "\""

{-# INLINE srcdoc_ #-}
srcdoc_ :: Convert a => a -> Attribute
srcdoc_ x = Attribute $ " srcdoc=\"" <> unConv (conv x) <> "\""

{-# INLINE srclang_ #-}
srclang_ :: Convert a => a -> Attribute
srclang_ x = Attribute $ " srclang=\"" <> unConv (conv x) <> "\""

{-# INLINE srcset_ #-}
srcset_ :: Convert a => a -> Attribute
srcset_ x = Attribute $ " srcset=\"" <> unConv (conv x) <> "\""

{-# INLINE start_ #-}
start_ :: Convert a => a -> Attribute
start_ x = Attribute $ " start=\"" <> unConv (conv x) <> "\""

{-# INLINE step_ #-}
step_ :: Convert a => a -> Attribute
step_ x = Attribute $ " step=\"" <> unConv (conv x) <> "\""

{-# INLINE style_ #-}
style_ :: Convert a => a -> Attribute
style_ x = Attribute $ " style=\"" <> unConv (conv x) <> "\""

{-# INLINE summary_ #-}
summary_ :: Convert a => a -> Attribute
summary_ x = Attribute $ " summary=\"" <> unConv (conv x) <> "\""

{-# INLINE tabindex_ #-}
tabindex_ :: Convert a => a -> Attribute
tabindex_ x = Attribute $ " tabindex=\"" <> unConv (conv x) <> "\""

{-# INLINE target_ #-}
target_ :: Convert a => a -> Attribute
target_ x = Attribute $ " target=\"" <> unConv (conv x) <> "\""

{-# INLINE title_ #-}
title_ :: Convert a => a -> Attribute
title_ x = Attribute $ " title=\"" <> unConv (conv x) <> "\""

{-# INLINE type_ #-}
type_ :: Convert a => a -> Attribute
type_ x = Attribute $ " type=\"" <> unConv (conv x) <> "\""

{-# INLINE usemap_ #-}
usemap_ :: Convert a => a -> Attribute
usemap_ x = Attribute $ " usemap=\"" <> unConv (conv x) <> "\""

{-# INLINE value_ #-}
value_ :: Convert a => a -> Attribute
value_ x = Attribute $ " value=\"" <> unConv (conv x) <> "\""

{-# INLINE width_ #-}
width_ :: Convert a => a -> Attribute
width_ x = Attribute $ " width=\"" <> unConv (conv x) <> "\""

{-# INLINE wrap_ #-}
wrap_ :: Convert a => a -> Attribute
wrap_ x = Attribute $ " wrap=\"" <> unConv (conv x) <> "\""
