{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Attribute where

import Html.Type

accept_ :: a -> 'AcceptA := a
accept_ = AT

acceptCharset_ :: a -> 'AcceptCharsetA := a
acceptCharset_ = AT

accesskey_ :: a -> 'AccesskeyA := a
accesskey_ = AT

action_ :: a -> 'ActionA := a
action_ = AT

align_ :: a -> 'AlignA := a
align_ = AT

alt_ :: a -> 'AltA := a
alt_ = AT

async_ :: a -> 'AsyncA := a
async_ = AT

autocomplete_ :: a -> 'AutocompleteA := a
autocomplete_ = AT

autofocus_ :: a -> 'AutofocusA := a
autofocus_ = AT

autoplay_ :: a -> 'AutoplayA := a
autoplay_ = AT

autosave_ :: a -> 'AutosaveA := a
autosave_ = AT

bgcolor_ :: a -> 'BgcolorA := a
bgcolor_ = AT

border_ :: a -> 'BorderA := a
border_ = AT

buffered_ :: a -> 'BufferedA := a
buffered_ = AT

challenge_ :: a -> 'ChallengeA := a
challenge_ = AT

charset_ :: a -> 'CharsetA := a
charset_ = AT

checked_ :: a -> 'CheckedA := a
checked_ = AT

cite_ :: a -> 'CiteA := a
cite_ = AT

class_ :: a -> 'ClassA := a
class_ = AT

code_ :: a -> 'CodeA := a
code_ = AT

codebase_ :: a -> 'CodebaseA := a
codebase_ = AT

color_ :: a -> 'ColorA := a
color_ = AT

cols_ :: a -> 'ColsA := a
cols_ = AT

colspan_ :: a -> 'ColspanA := a
colspan_ = AT

content_ :: a -> 'ContentA := a
content_ = AT

contenteditable_ :: a -> 'ContenteditableA := a
contenteditable_ = AT

contextmenu_ :: a -> 'ContextmenuA := a
contextmenu_ = AT

controls_ :: a -> 'ControlsA := a
controls_ = AT

coords_ :: a -> 'CoordsA := a
coords_ = AT

crossorigin_ :: a -> 'CrossoriginA := a
crossorigin_ = AT

data_ :: a -> 'DataA := a
data_ = AT

datetime_ :: a -> 'DatetimeA := a
datetime_ = AT

default_ :: a -> 'DefaultA := a
default_ = AT

defer_ :: a -> 'DeferA := a
defer_ = AT

dir_ :: a -> 'DirA := a
dir_ = AT

dirname_ :: a -> 'DirnameA := a
dirname_ = AT

disabled_ :: a -> 'DisabledA := a
disabled_ = AT

download_ :: a -> 'DownloadA := a
download_ = AT

draggable_ :: a -> 'DraggableA := a
draggable_ = AT

dropzone_ :: a -> 'DropzoneA := a
dropzone_ = AT

enctype_ :: a -> 'EnctypeA := a
enctype_ = AT

for_ :: a -> 'ForA := a
for_ = AT

form_ :: a -> 'FormA := a
form_ = AT

formaction_ :: a -> 'FormactionA := a
formaction_ = AT

headers_ :: a -> 'HeadersA := a
headers_ = AT

height_ :: a -> 'HeightA := a
height_ = AT

hidden_ :: a -> 'HiddenA := a
hidden_ = AT

high_ :: a -> 'HighA := a
high_ = AT

href_ :: a -> 'HrefA := a
href_ = AT

hreflang_ :: a -> 'HreflangA := a
hreflang_ = AT

httpEquiv_ :: a -> 'HttpEquivA := a
httpEquiv_ = AT

icon_ :: a -> 'IconA := a
icon_ = AT

id_ :: a -> 'IdA := a
id_ = AT

integrity_ :: a -> 'IntegrityA := a
integrity_ = AT

ismap_ :: a -> 'IsmapA := a
ismap_ = AT

itemprop_ :: a -> 'ItempropA := a
itemprop_ = AT

keytype_ :: a -> 'KeytypeA := a
keytype_ = AT

kind_ :: a -> 'KindA := a
kind_ = AT

label_ :: a -> 'LabelA := a
label_ = AT

lang_ :: a -> 'LangA := a
lang_ = AT

language_ :: a -> 'LanguageA := a
language_ = AT

list_ :: a -> 'ListA := a
list_ = AT

loop_ :: a -> 'LoopA := a
loop_ = AT

low_ :: a -> 'LowA := a
low_ = AT

manifest_ :: a -> 'ManifestA := a
manifest_ = AT

max_ :: a -> 'MaxA := a
max_ = AT

maxlength_ :: a -> 'MaxlengthA := a
maxlength_ = AT

minlength_ :: a -> 'MinlengthA := a
minlength_ = AT

media_ :: a -> 'MediaA := a
media_ = AT

method_ :: a -> 'MethodA := a
method_ = AT

min_ :: a -> 'MinA := a
min_ = AT

multiple_ :: a -> 'MultipleA := a
multiple_ = AT

muted_ :: a -> 'MutedA := a
muted_ = AT

name_ :: a -> 'NameA := a
name_ = AT

novalidate_ :: a -> 'NovalidateA := a
novalidate_ = AT

open_ :: a -> 'OpenA := a
open_ = AT

optimum_ :: a -> 'OptimumA := a
optimum_ = AT

pattern_ :: a -> 'PatternA := a
pattern_ = AT

ping_ :: a -> 'PingA := a
ping_ = AT

placeholder_ :: a -> 'PlaceholderA := a
placeholder_ = AT

poster_ :: a -> 'PosterA := a
poster_ = AT

preload_ :: a -> 'PreloadA := a
preload_ = AT

radiogroup_ :: a -> 'RadiogroupA := a
radiogroup_ = AT

readonly_ :: a -> 'ReadonlyA := a
readonly_ = AT

rel_ :: a -> 'RelA := a
rel_ = AT

required_ :: a -> 'RequiredA := a
required_ = AT

reversed_ :: a -> 'ReversedA := a
reversed_ = AT

rows_ :: a -> 'RowsA := a
rows_ = AT

rowspan_ :: a -> 'RowspanA := a
rowspan_ = AT

sandbox_ :: a -> 'SandboxA := a
sandbox_ = AT

scope_ :: a -> 'ScopeA := a
scope_ = AT

scoped_ :: a -> 'ScopedA := a
scoped_ = AT

seamless_ :: a -> 'SeamlessA := a
seamless_ = AT

selected_ :: a -> 'SelectedA := a
selected_ = AT

shape_ :: a -> 'ShapeA := a
shape_ = AT

size_ :: a -> 'SizeA := a
size_ = AT

sizes_ :: a -> 'SizesA := a
sizes_ = AT

slot_ :: a -> 'SlotA := a
slot_ = AT

span_ :: a -> 'SpanA := a
span_ = AT

spellcheck_ :: a -> 'SpellcheckA := a
spellcheck_ = AT

src_ :: a -> 'SrcA := a
src_ = AT

srcdoc_ :: a -> 'SrcdocA := a
srcdoc_ = AT

srclang_ :: a -> 'SrclangA := a
srclang_ = AT

srcset_ :: a -> 'SrcsetA := a
srcset_ = AT

start_ :: a -> 'StartA := a
start_ = AT

step_ :: a -> 'StepA := a
step_ = AT

style_ :: a -> 'StyleA := a
style_ = AT

summary_ :: a -> 'SummaryA := a
summary_ = AT

tabindex_ :: a -> 'TabindexA := a
tabindex_ = AT

target_ :: a -> 'TargetA := a
target_ = AT

title_ :: a -> 'TitleA := a
title_ = AT

type_ :: a -> 'TypeA := a
type_ = AT

usemap_ :: a -> 'UsemapA := a
usemap_ = AT

value_ :: a -> 'ValueA := a
value_ = AT

width_ :: a -> 'WidthA := a
width_ = AT

wrap_ :: a -> 'WrapA := a
wrap_ = AT

addAttributes :: (a ??> b, a ?> c) => b -> a > c -> (a :@: b) c
addAttributes b (Child c) = WithAttributes b c
