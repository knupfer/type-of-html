{-# LANGUAGE OverloadedStrings #-}

module Html.Attribute where

import Html.Convert
import Html.Type
import Data.Semigroup

{-# INLINE accept_ #-}
accept_ :: Convert a => a -> Attribute
accept_ x = Attribute $ " accept=\"" <> conv x <> "\""

{-# INLINE acceptcharset_ #-}
acceptcharset_ :: Convert a => a -> Attribute
acceptcharset_ x = Attribute $ " acceptcharset=\"" <> conv x <> "\""

{-# INLINE accesskey_ #-}
accesskey_ :: Convert a => a -> Attribute
accesskey_ x = Attribute $ " accesskey=\"" <> conv x <> "\""

{-# INLINE action_ #-}
action_ :: Convert a => a -> Attribute
action_ x = Attribute $ " action=\"" <> conv x <> "\""

{-# INLINE align_ #-}
align_ :: Convert a => a -> Attribute
align_ x = Attribute $ " align=\"" <> conv x <> "\""

{-# INLINE alt_ #-}
alt_ :: Convert a => a -> Attribute
alt_ x = Attribute $ " alt=\"" <> conv x <> "\""

{-# INLINE async_ #-}
async_ :: Convert a => a -> Attribute
async_ x = Attribute $ " async=\"" <> conv x <> "\""

{-# INLINE autocomplete_ #-}
autocomplete_ :: Convert a => a -> Attribute
autocomplete_ x = Attribute $ " autocomplete=\"" <> conv x <> "\""

{-# INLINE autofocus_ #-}
autofocus_ :: Convert a => a -> Attribute
autofocus_ x = Attribute $ " autofocus=\"" <> conv x <> "\""

{-# INLINE autoplay_ #-}
autoplay_ :: Convert a => a -> Attribute
autoplay_ x = Attribute $ " autoplay=\"" <> conv x <> "\""

{-# INLINE autosave_ #-}
autosave_ :: Convert a => a -> Attribute
autosave_ x = Attribute $ " autosave=\"" <> conv x <> "\""

{-# INLINE bgcolor_ #-}
bgcolor_ :: Convert a => a -> Attribute
bgcolor_ x = Attribute $ " bgcolor=\"" <> conv x <> "\""

{-# INLINE border_ #-}
border_ :: Convert a => a -> Attribute
border_ x = Attribute $ " border=\"" <> conv x <> "\""

{-# INLINE buffered_ #-}
buffered_ :: Convert a => a -> Attribute
buffered_ x = Attribute $ " buffered=\"" <> conv x <> "\""

{-# INLINE challenge_ #-}
challenge_ :: Convert a => a -> Attribute
challenge_ x = Attribute $ " challenge=\"" <> conv x <> "\""

{-# INLINE charset_ #-}
charset_ :: Convert a => a -> Attribute
charset_ x = Attribute $ " charset=\"" <> conv x <> "\""

{-# INLINE checked_ #-}
checked_ :: Convert a => a -> Attribute
checked_ x = Attribute $ " checked=\"" <> conv x <> "\""

{-# INLINE cite_ #-}
cite_ :: Convert a => a -> Attribute
cite_ x = Attribute $ " cite=\"" <> conv x <> "\""

{-# INLINE class_ #-}
class_ :: Convert a => a -> Attribute
class_ x = Attribute $ " class=\"" <> conv x <> "\""

{-# INLINE code_ #-}
code_ :: Convert a => a -> Attribute
code_ x = Attribute $ " code=\"" <> conv x <> "\""

{-# INLINE codebase_ #-}
codebase_ :: Convert a => a -> Attribute
codebase_ x = Attribute $ " codebase=\"" <> conv x <> "\""

{-# INLINE color_ #-}
color_ :: Convert a => a -> Attribute
color_ x = Attribute $ " color=\"" <> conv x <> "\""

{-# INLINE cols_ #-}
cols_ :: Convert a => a -> Attribute
cols_ x = Attribute $ " cols=\"" <> conv x <> "\""

{-# INLINE colspan_ #-}
colspan_ :: Convert a => a -> Attribute
colspan_ x = Attribute $ " colspan=\"" <> conv x <> "\""

{-# INLINE content_ #-}
content_ :: Convert a => a -> Attribute
content_ x = Attribute $ " content=\"" <> conv x <> "\""

{-# INLINE contenteditable_ #-}
contenteditable_ :: Convert a => a -> Attribute
contenteditable_ x = Attribute $ " contenteditable=\"" <> conv x <> "\""

{-# INLINE contextmenu_ #-}
contextmenu_ :: Convert a => a -> Attribute
contextmenu_ x = Attribute $ " contextmenu=\"" <> conv x <> "\""

{-# INLINE controls_ #-}
controls_ :: Convert a => a -> Attribute
controls_ x = Attribute $ " controls=\"" <> conv x <> "\""

{-# INLINE coords_ #-}
coords_ :: Convert a => a -> Attribute
coords_ x = Attribute $ " coords=\"" <> conv x <> "\""

{-# INLINE crossorigin_ #-}
crossorigin_ :: Convert a => a -> Attribute
crossorigin_ x = Attribute $ " crossorigin=\"" <> conv x <> "\""

{-# INLINE data_ #-}
data_ :: Convert a => a -> Attribute
data_ x = Attribute $ " data=\"" <> conv x <> "\""

{-# INLINE datetime_ #-}
datetime_ :: Convert a => a -> Attribute
datetime_ x = Attribute $ " datetime=\"" <> conv x <> "\""

{-# INLINE default_ #-}
default_ :: Convert a => a -> Attribute
default_ x = Attribute $ " default=\"" <> conv x <> "\""

{-# INLINE defer_ #-}
defer_ :: Convert a => a -> Attribute
defer_ x = Attribute $ " defer=\"" <> conv x <> "\""

{-# INLINE dir_ #-}
dir_ :: Convert a => a -> Attribute
dir_ x = Attribute $ " dir=\"" <> conv x <> "\""

{-# INLINE dirname_ #-}
dirname_ :: Convert a => a -> Attribute
dirname_ x = Attribute $ " dirname=\"" <> conv x <> "\""

{-# INLINE disabled_ #-}
disabled_ :: Convert a => a -> Attribute
disabled_ x = Attribute $ " disabled=\"" <> conv x <> "\""

{-# INLINE download_ #-}
download_ :: Convert a => a -> Attribute
download_ x = Attribute $ " download=\"" <> conv x <> "\""

{-# INLINE draggable_ #-}
draggable_ :: Convert a => a -> Attribute
draggable_ x = Attribute $ " draggable=\"" <> conv x <> "\""

{-# INLINE dropzone_ #-}
dropzone_ :: Convert a => a -> Attribute
dropzone_ x = Attribute $ " dropzone=\"" <> conv x <> "\""

{-# INLINE enctype_ #-}
enctype_ :: Convert a => a -> Attribute
enctype_ x = Attribute $ " enctype=\"" <> conv x <> "\""

{-# INLINE for_ #-}
for_ :: Convert a => a -> Attribute
for_ x = Attribute $ " for=\"" <> conv x <> "\""

{-# INLINE form_ #-}
form_ :: Convert a => a -> Attribute
form_ x = Attribute $ " form=\"" <> conv x <> "\""

{-# INLINE formaction_ #-}
formaction_ :: Convert a => a -> Attribute
formaction_ x = Attribute $ " formaction=\"" <> conv x <> "\""

{-# INLINE headers_ #-}
headers_ :: Convert a => a -> Attribute
headers_ x = Attribute $ " headers=\"" <> conv x <> "\""

{-# INLINE height_ #-}
height_ :: Convert a => a -> Attribute
height_ x = Attribute $ " height=\"" <> conv x <> "\""

{-# INLINE hidden_ #-}
hidden_ :: Convert a => a -> Attribute
hidden_ x = Attribute $ " hidden=\"" <> conv x <> "\""

{-# INLINE high_ #-}
high_ :: Convert a => a -> Attribute
high_ x = Attribute $ " high=\"" <> conv x <> "\""

{-# INLINE href_ #-}
href_ :: Convert a => a -> Attribute
href_ x = Attribute $ " href=\"" <> conv x <> "\""

{-# INLINE hreflang_ #-}
hreflang_ :: Convert a => a -> Attribute
hreflang_ x = Attribute $ " hreflang=\"" <> conv x <> "\""

{-# INLINE httpequiv_ #-}
httpequiv_ :: Convert a => a -> Attribute
httpequiv_ x = Attribute $ " httpequiv=\"" <> conv x <> "\""

{-# INLINE icon_ #-}
icon_ :: Convert a => a -> Attribute
icon_ x = Attribute $ " icon=\"" <> conv x <> "\""

{-# INLINE id_ #-}
id_ :: Convert a => a -> Attribute
id_ x = Attribute $ " id=\"" <> conv x <> "\""

{-# INLINE integrity_ #-}
integrity_ :: Convert a => a -> Attribute
integrity_ x = Attribute $ " integrity=\"" <> conv x <> "\""

{-# INLINE ismap_ #-}
ismap_ :: Convert a => a -> Attribute
ismap_ x = Attribute $ " ismap=\"" <> conv x <> "\""

{-# INLINE itemprop_ #-}
itemprop_ :: Convert a => a -> Attribute
itemprop_ x = Attribute $ " itemprop=\"" <> conv x <> "\""

{-# INLINE keytype_ #-}
keytype_ :: Convert a => a -> Attribute
keytype_ x = Attribute $ " keytype=\"" <> conv x <> "\""

{-# INLINE kind_ #-}
kind_ :: Convert a => a -> Attribute
kind_ x = Attribute $ " kind=\"" <> conv x <> "\""

{-# INLINE label_ #-}
label_ :: Convert a => a -> Attribute
label_ x = Attribute $ " label=\"" <> conv x <> "\""

{-# INLINE lang_ #-}
lang_ :: Convert a => a -> Attribute
lang_ x = Attribute $ " lang=\"" <> conv x <> "\""

{-# INLINE language_ #-}
language_ :: Convert a => a -> Attribute
language_ x = Attribute $ " language=\"" <> conv x <> "\""

{-# INLINE list_ #-}
list_ :: Convert a => a -> Attribute
list_ x = Attribute $ " list=\"" <> conv x <> "\""

{-# INLINE loop_ #-}
loop_ :: Convert a => a -> Attribute
loop_ x = Attribute $ " loop=\"" <> conv x <> "\""

{-# INLINE low_ #-}
low_ :: Convert a => a -> Attribute
low_ x = Attribute $ " low=\"" <> conv x <> "\""

{-# INLINE manifest_ #-}
manifest_ :: Convert a => a -> Attribute
manifest_ x = Attribute $ " manifest=\"" <> conv x <> "\""

{-# INLINE max_ #-}
max_ :: Convert a => a -> Attribute
max_ x = Attribute $ " max=\"" <> conv x <> "\""

{-# INLINE maxlength_ #-}
maxlength_ :: Convert a => a -> Attribute
maxlength_ x = Attribute $ " maxlength=\"" <> conv x <> "\""

{-# INLINE minlength_ #-}
minlength_ :: Convert a => a -> Attribute
minlength_ x = Attribute $ " minlength=\"" <> conv x <> "\""

{-# INLINE media_ #-}
media_ :: Convert a => a -> Attribute
media_ x = Attribute $ " media=\"" <> conv x <> "\""

{-# INLINE method_ #-}
method_ :: Convert a => a -> Attribute
method_ x = Attribute $ " method=\"" <> conv x <> "\""

{-# INLINE min_ #-}
min_ :: Convert a => a -> Attribute
min_ x = Attribute $ " min=\"" <> conv x <> "\""

{-# INLINE multiple_ #-}
multiple_ :: Convert a => a -> Attribute
multiple_ x = Attribute $ " multiple=\"" <> conv x <> "\""

{-# INLINE muted_ #-}
muted_ :: Convert a => a -> Attribute
muted_ x = Attribute $ " muted=\"" <> conv x <> "\""

{-# INLINE name_ #-}
name_ :: Convert a => a -> Attribute
name_ x = Attribute $ " name=\"" <> conv x <> "\""

{-# INLINE novalidate_ #-}
novalidate_ :: Convert a => a -> Attribute
novalidate_ x = Attribute $ " novalidate=\"" <> conv x <> "\""

{-# INLINE open_ #-}
open_ :: Convert a => a -> Attribute
open_ x = Attribute $ " open=\"" <> conv x <> "\""

{-# INLINE optimum_ #-}
optimum_ :: Convert a => a -> Attribute
optimum_ x = Attribute $ " optimum=\"" <> conv x <> "\""

{-# INLINE pattern_ #-}
pattern_ :: Convert a => a -> Attribute
pattern_ x = Attribute $ " pattern=\"" <> conv x <> "\""

{-# INLINE ping_ #-}
ping_ :: Convert a => a -> Attribute
ping_ x = Attribute $ " ping=\"" <> conv x <> "\""

{-# INLINE placeholder_ #-}
placeholder_ :: Convert a => a -> Attribute
placeholder_ x = Attribute $ " placeholder=\"" <> conv x <> "\""

{-# INLINE poster_ #-}
poster_ :: Convert a => a -> Attribute
poster_ x = Attribute $ " poster=\"" <> conv x <> "\""

{-# INLINE preload_ #-}
preload_ :: Convert a => a -> Attribute
preload_ x = Attribute $ " preload=\"" <> conv x <> "\""

{-# INLINE radiogroup_ #-}
radiogroup_ :: Convert a => a -> Attribute
radiogroup_ x = Attribute $ " radiogroup=\"" <> conv x <> "\""

{-# INLINE readonly_ #-}
readonly_ :: Convert a => a -> Attribute
readonly_ x = Attribute $ " readonly=\"" <> conv x <> "\""

{-# INLINE rel_ #-}
rel_ :: Convert a => a -> Attribute
rel_ x = Attribute $ " rel=\"" <> conv x <> "\""

{-# INLINE required_ #-}
required_ :: Convert a => a -> Attribute
required_ x = Attribute $ " required=\"" <> conv x <> "\""

{-# INLINE reversed_ #-}
reversed_ :: Convert a => a -> Attribute
reversed_ x = Attribute $ " reversed=\"" <> conv x <> "\""

{-# INLINE rows_ #-}
rows_ :: Convert a => a -> Attribute
rows_ x = Attribute $ " rows=\"" <> conv x <> "\""

{-# INLINE rowspan_ #-}
rowspan_ :: Convert a => a -> Attribute
rowspan_ x = Attribute $ " rowspan=\"" <> conv x <> "\""

{-# INLINE sandbox_ #-}
sandbox_ :: Convert a => a -> Attribute
sandbox_ x = Attribute $ " sandbox=\"" <> conv x <> "\""

{-# INLINE scope_ #-}
scope_ :: Convert a => a -> Attribute
scope_ x = Attribute $ " scope=\"" <> conv x <> "\""

{-# INLINE scoped_ #-}
scoped_ :: Convert a => a -> Attribute
scoped_ x = Attribute $ " scoped=\"" <> conv x <> "\""

{-# INLINE seamless_ #-}
seamless_ :: Convert a => a -> Attribute
seamless_ x = Attribute $ " seamless=\"" <> conv x <> "\""

{-# INLINE selected_ #-}
selected_ :: Convert a => a -> Attribute
selected_ x = Attribute $ " selected=\"" <> conv x <> "\""

{-# INLINE shape_ #-}
shape_ :: Convert a => a -> Attribute
shape_ x = Attribute $ " shape=\"" <> conv x <> "\""

{-# INLINE size_ #-}
size_ :: Convert a => a -> Attribute
size_ x = Attribute $ " size=\"" <> conv x <> "\""

{-# INLINE sizes_ #-}
sizes_ :: Convert a => a -> Attribute
sizes_ x = Attribute $ " sizes=\"" <> conv x <> "\""

{-# INLINE slot_ #-}
slot_ :: Convert a => a -> Attribute
slot_ x = Attribute $ " slot=\"" <> conv x <> "\""

{-# INLINE span_ #-}
span_ :: Convert a => a -> Attribute
span_ x = Attribute $ " span=\"" <> conv x <> "\""

{-# INLINE spellcheck_ #-}
spellcheck_ :: Convert a => a -> Attribute
spellcheck_ x = Attribute $ " spellcheck=\"" <> conv x <> "\""

{-# INLINE src_ #-}
src_ :: Convert a => a -> Attribute
src_ x = Attribute $ " src=\"" <> conv x <> "\""

{-# INLINE srcdoc_ #-}
srcdoc_ :: Convert a => a -> Attribute
srcdoc_ x = Attribute $ " srcdoc=\"" <> conv x <> "\""

{-# INLINE srclang_ #-}
srclang_ :: Convert a => a -> Attribute
srclang_ x = Attribute $ " srclang=\"" <> conv x <> "\""

{-# INLINE srcset_ #-}
srcset_ :: Convert a => a -> Attribute
srcset_ x = Attribute $ " srcset=\"" <> conv x <> "\""

{-# INLINE start_ #-}
start_ :: Convert a => a -> Attribute
start_ x = Attribute $ " start=\"" <> conv x <> "\""

{-# INLINE step_ #-}
step_ :: Convert a => a -> Attribute
step_ x = Attribute $ " step=\"" <> conv x <> "\""

{-# INLINE style_ #-}
style_ :: Convert a => a -> Attribute
style_ x = Attribute $ " style=\"" <> conv x <> "\""

{-# INLINE summary_ #-}
summary_ :: Convert a => a -> Attribute
summary_ x = Attribute $ " summary=\"" <> conv x <> "\""

{-# INLINE tabindex_ #-}
tabindex_ :: Convert a => a -> Attribute
tabindex_ x = Attribute $ " tabindex=\"" <> conv x <> "\""

{-# INLINE target_ #-}
target_ :: Convert a => a -> Attribute
target_ x = Attribute $ " target=\"" <> conv x <> "\""

{-# INLINE title_ #-}
title_ :: Convert a => a -> Attribute
title_ x = Attribute $ " title=\"" <> conv x <> "\""

{-# INLINE type_ #-}
type_ :: Convert a => a -> Attribute
type_ x = Attribute $ " type=\"" <> conv x <> "\""

{-# INLINE usemap_ #-}
usemap_ :: Convert a => a -> Attribute
usemap_ x = Attribute $ " usemap=\"" <> conv x <> "\""

{-# INLINE value_ #-}
value_ :: Convert a => a -> Attribute
value_ x = Attribute $ " value=\"" <> conv x <> "\""

{-# INLINE width_ #-}
width_ :: Convert a => a -> Attribute
width_ x = Attribute $ " width=\"" <> conv x <> "\""

{-# INLINE wrap_ #-}
wrap_ :: Convert a => a -> Attribute
wrap_ x = Attribute $ " wrap=\"" <> conv x <> "\""
