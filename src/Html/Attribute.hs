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

allowfullscreen_ :: 'AllowfullscreenA := ()
allowfullscreen_ = AT ()

allowpaymentrequest_ :: 'AllowpaymentrequestA := ()
allowpaymentrequest_ = AT ()

align_ :: a -> 'AlignA := a
align_ = AT

alt_ :: a -> 'AltA := a
alt_ = AT

async_ :: 'AsyncA := ()
async_ = AT ()

autocomplete_ :: a -> 'AutocompleteA := a
autocomplete_ = AT

autofocus_ :: 'AutofocusA := ()
autofocus_ = AT ()

autoplay_ :: 'AutoplayA := ()
autoplay_ = AT ()

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

checked_ :: 'CheckedA := ()
checked_ = AT ()

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

cols_ :: Integral a => a -> 'ColsA := a
cols_ = AT

colspan_ :: Integral a => a -> 'ColspanA := a
colspan_ = AT

content_ :: a -> 'ContentA := a
content_ = AT

contenteditable_ :: a -> 'ContenteditableA := a
contenteditable_ = AT

contextmenu_ :: a -> 'ContextmenuA := a
contextmenu_ = AT

controls_ :: 'ControlsA := ()
controls_ = AT ()

coords_ :: a -> 'CoordsA := a
coords_ = AT

crossorigin_ :: a -> 'CrossoriginA := a
crossorigin_ = AT

data_ :: a -> 'DataA := a
data_ = AT

datetime_ :: a -> 'DatetimeA := a
datetime_ = AT

default_ :: 'DefaultA := ()
default_ = AT ()

defer_ :: 'DeferA := ()
defer_ = AT ()

dir_ :: a -> 'DirA := a
dir_ = AT

dirname_ :: a -> 'DirnameA := a
dirname_ = AT

disabled_ :: 'DisabledA := ()
disabled_ = AT ()

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

formenctype_ :: a -> 'FormenctypeA := a
formenctype_ = AT

formmethod_ :: a -> 'FormmethodA := a
formmethod_ = AT

formnovalidate_ :: 'FormnovalidateA := ()
formnovalidate_ = AT ()

formtarget_ :: a -> 'FormtargetA := a
formtarget_ = AT

headers_ :: a -> 'HeadersA := a
headers_ = AT

height_ :: Integral a => a -> 'HeightA := a
height_ = AT

hidden_ :: 'HiddenA := ()
hidden_ = AT ()

high_ :: Num a => a -> 'HighA := a
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

ismap_ :: 'IsmapA := ()
ismap_ = AT ()

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

longdesc_ :: a -> 'LongdescA := a
longdesc_ = AT

loop_ :: 'LoopA := ()
loop_ = AT ()

low_ :: Num a => a -> 'LowA := a
low_ = AT

manifest_ :: a -> 'ManifestA := a
manifest_ = AT

max_ :: Num a => a -> 'MaxA := a
max_ = AT

maxlength_ :: Integral a => a -> 'MaxlengthA := a
maxlength_ = AT

minlength_ :: Integral a => a -> 'MinlengthA := a
minlength_ = AT

media_ :: a -> 'MediaA := a
media_ = AT

method_ :: a -> 'MethodA := a
method_ = AT

min_ :: Num a => a -> 'MinA := a
min_ = AT

multiple_ :: 'MultipleA := ()
multiple_ = AT ()

muted_ :: 'MutedA := ()
muted_ = AT ()

name_ :: a -> 'NameA := a
name_ = AT

nonce_ :: a -> 'NonceA := a
nonce_ = AT

novalidate_ :: 'NovalidateA := ()
novalidate_ = AT ()

open_ :: 'OpenA := ()
open_ = AT ()

optimum_ :: Num a => a -> 'OptimumA := a
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

readonly_ :: 'ReadonlyA := ()
readonly_ = AT ()

referrerpolicy_ :: a -> 'ReferrerpolicyA := a
referrerpolicy_ = AT

rel_ :: a -> 'RelA := a
rel_ = AT

required_ :: 'RequiredA := ()
required_ = AT ()

rev_ :: a -> 'RevA := a
rev_ = AT

reversed_ :: 'ReversedA := ()
reversed_ = AT ()

rows_ :: Integral a => a -> 'RowsA := a
rows_ = AT

rowspan_ :: Integral a => a -> 'RowspanA := a
rowspan_ = AT

sandbox_ :: a -> 'SandboxA := a
sandbox_ = AT

scope_ :: a -> 'ScopeA := a
scope_ = AT

scoped_ :: a -> 'ScopedA := a
scoped_ = AT

seamless_ :: a -> 'SeamlessA := a
seamless_ = AT

selected_ :: 'SelectedA := ()
selected_ = AT ()

shape_ :: a -> 'ShapeA := a
shape_ = AT

size_ :: Integral a => a -> 'SizeA := a
size_ = AT

sizes_ :: a -> 'SizesA := a
sizes_ = AT

slot_ :: a -> 'SlotA := a
slot_ = AT

span_ :: Integral a => a -> 'SpanA := a
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

start_ :: Integral a => a -> 'StartA := a
start_ = AT

step_ :: Num a => a -> 'StepA := a
step_ = AT

style_ :: a -> 'StyleA := a
style_ = AT

summary_ :: a -> 'SummaryA := a
summary_ = AT

tabindex_ :: Integral a => a -> 'TabindexA := a
tabindex_ = AT

target_ :: a -> 'TargetA := a
target_ = AT

title_ :: a -> 'TitleA := a
title_ = AT

translate_ :: a -> 'TranslateA := a
translate_ = AT

type_ :: a -> 'TypeA := a
type_ = AT

typemustmatch_ :: 'TypemustmatchA := ()
typemustmatch_ = AT ()

usemap_ :: a -> 'UsemapA := a
usemap_ = AT

value_ :: a -> 'ValueA := a
value_ = AT

width_ :: Integral a => a -> 'WidthA := a
width_ = AT

wrap_ :: a -> 'WrapA := a
wrap_ = AT

addAttributes :: (a <?> (b # b')) c => b' -> (a :@: b) c -> (a :@: (b # b')) c
addAttributes b' (WithAttributes b c) = WithAttributes (b # b') c
