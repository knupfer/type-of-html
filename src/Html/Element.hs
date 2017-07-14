{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Element where

import Html.Type

a_ :: ('A ?> a) => a -> 'A > a
a_ = Child

abbr_ :: ('Abbr ?> a) => a -> 'Abbr > a
abbr_ = Child

acronym_ :: ('Acronym ?> a) => a -> 'Acronym > a
acronym_ = Child

address_ :: ('Address ?> a) => a -> 'Address > a
address_ = Child

applet_ :: ('Applet ?> a) => a -> 'Applet > a
applet_ = Child

area_ :: 'Area > ()
area_ = Child ()

article_ :: ('Article ?> a) => a -> 'Article > a
article_ = Child

aside_ :: ('Aside ?> a) => a -> 'Aside > a
aside_ = Child

audio_ :: ('Audio ?> a) => a -> 'Audio > a
audio_ = Child

b_ :: ('B ?> a) => a -> 'B > a
b_ = Child

base_ :: 'Base > ()
base_ = Child ()

basefont_ :: ('Basefont ?> a) => a -> 'Basefont > a
basefont_ = Child

bdi_ :: ('Bdi ?> a) => a -> 'Bdi > a
bdi_ = Child

bdo_ :: ('Bdo ?> a) => a -> 'Bdo > a
bdo_ = Child

bgsound_ :: ('Bgsound ?> a) => a -> 'Bgsound > a
bgsound_ = Child

big_ :: ('Big ?> a) => a -> 'Big > a
big_ = Child

blink_ :: ('Blink ?> a) => a -> 'Blink > a
blink_ = Child

blockquote_ :: ('Blockquote ?> a) => a -> 'Blockquote > a
blockquote_ = Child

body_ :: ('Body ?> a) => a -> 'Body > a
body_ = Child

br_ :: 'Br > ()
br_ = Child ()

button_ :: ('Button ?> a) => a -> 'Button > a
button_ = Child

canvas_ :: ('Canvas ?> a) => a -> 'Canvas > a
canvas_ = Child

caption_ :: ('Caption ?> a) => a -> 'Caption > a
caption_ = Child

center_ :: ('Center ?> a) => a -> 'Center > a
center_ = Child

cite_ :: ('Cite ?> a) => a -> 'Cite > a
cite_ = Child

code_ :: ('Code ?> a) => a -> 'Code > a
code_ = Child

col_ :: 'Col > ()
col_ = Child ()

colgroup_ :: ('Colgroup ?> a) => a -> 'Colgroup > a
colgroup_ = Child

command_ :: ('Command ?> a) => a -> 'Command > a
command_ = Child

content_ :: ('Content ?> a) => a -> 'Content > a
content_ = Child

data_ :: ('Data ?> a) => a -> 'Data > a
data_ = Child

datalist_ :: ('Datalist ?> a) => a -> 'Datalist > a
datalist_ = Child

dd_ :: ('Dd ?> a) => a -> 'Dd > a
dd_ = Child

del_ :: ('Del ?> a) => a -> 'Del > a
del_ = Child

details_ :: ('Details ?> a) => a -> 'Details > a
details_ = Child

dfn_ :: ('Dfn ?> a) => a -> 'Dfn > a
dfn_ = Child

dialog_ :: ('Dialog ?> a) => a -> 'Dialog > a
dialog_ = Child

dir_ :: ('Dir ?> a) => a -> 'Dir > a
dir_ = Child

div_ :: ('Div ?> a) => a -> 'Div > a
div_ = Child

dl_ :: ('Dl ?> a) => a -> 'Dl > a
dl_ = Child

dt_ :: ('Dt ?> a) => a -> 'Dt > a
dt_ = Child

element_ :: ('Element ?> a) => a -> 'Element > a
element_ = Child

em_ :: ('Em ?> a) => a -> 'Em > a
em_ = Child

embed_ :: 'Embed > ()
embed_ = Child ()

fieldset_ :: ('Fieldset ?> a) => a -> 'Fieldset > a
fieldset_ = Child

figcaption_ :: ('Figcaption ?> a) => a -> 'Figcaption > a
figcaption_ = Child

figure_ :: ('Figure ?> a) => a -> 'Figure > a
figure_ = Child

font_ :: ('Font ?> a) => a -> 'Font > a
font_ = Child

footer_ :: ('Footer ?> a) => a -> 'Footer > a
footer_ = Child

form_ :: ('Form ?> a) => a -> 'Form > a
form_ = Child

frame_ :: ('Frame ?> a) => a -> 'Frame > a
frame_ = Child

frameset_ :: ('Frameset ?> a) => a -> 'Frameset > a
frameset_ = Child

h1_ :: ('H1 ?> a) => a -> 'H1 > a
h1_ = Child

h2_ :: ('H2 ?> a) => a -> 'H2 > a
h2_ = Child

h3_ :: ('H3 ?> a) => a -> 'H3 > a
h3_ = Child

h4_ :: ('H4 ?> a) => a -> 'H4 > a
h4_ = Child

h5_ :: ('H5 ?> a) => a -> 'H5 > a
h5_ = Child

h6_ :: ('H6 ?> a) => a -> 'H6 > a
h6_ = Child

head_ :: ('Head ?> a) => a -> 'Head > a
head_ = Child

header_ :: ('Header ?> a) => a -> 'Header > a
header_ = Child

hgroup_ :: ('Hgroup ?> a) => a -> 'Hgroup > a
hgroup_ = Child

hr_ :: 'Hr > ()
hr_ = Child ()

html_ :: ('Html ?> a) => a -> 'Html > a
html_ = Child

i_ :: ('I ?> a) => a -> 'I > a
i_ = Child

iframe_ :: 'Iframe > ()
iframe_ = Child ()

image_ :: ('Image ?> a) => a -> 'Image > a
image_ = Child

img_ :: 'Img > ()
img_ = Child ()

input_ :: ('Input ?> a) => a -> 'Input > a
input_ = Child

ins_ :: ('Ins ?> a) => a -> 'Ins > a
ins_ = Child

isindex_ :: ('Isindex ?> a) => a -> 'Isindex > a
isindex_ = Child

kbd_ :: ('Kbd ?> a) => a -> 'Kbd > a
kbd_ = Child

keygen_ :: ('Keygen ?> a) => a -> 'Keygen > a
keygen_ = Child

label_ :: ('Label ?> a) => a -> 'Label > a
label_ = Child

legend_ :: ('Legend ?> a) => a -> 'Legend > a
legend_ = Child

li_ :: ('Li ?> a) => a -> 'Li > a
li_ = Child

link_ :: 'Link > ()
link_ = Child ()

listing_ :: ('Listing ?> a) => a -> 'Listing > a
listing_ = Child

main_ :: ('Main ?> a) => a -> 'Main > a
main_ = Child

map_ :: ('Map ?> a) => a -> 'Map > a
map_ = Child

mark_ :: ('Mark ?> a) => a -> 'Mark > a
mark_ = Child

marquee_ :: ('Marquee ?> a) => a -> 'Marquee > a
marquee_ = Child

math_ :: ('Math ?> a) => a -> 'Math > a
math_ = Child

menu_ :: ('Menu ?> a) => a -> 'Menu > a
menu_ = Child

menuitem_ :: 'Menuitem > ()
menuitem_ = Child ()

meta_ :: 'Meta > ()
meta_ = Child ()

meter_ :: ('Meter ?> a) => a -> 'Meter > a
meter_ = Child

multicol_ :: ('Multicol ?> a) => a -> 'Multicol > a
multicol_ = Child

nav_ :: ('Nav ?> a) => a -> 'Nav > a
nav_ = Child

nextid_ :: ('Nextid ?> a) => a -> 'Nextid > a
nextid_ = Child

nobr_ :: ('Nobr ?> a) => a -> 'Nobr > a
nobr_ = Child

noembed_ :: ('Noembed ?> a) => a -> 'Noembed > a
noembed_ = Child

noframes_ :: ('Noframes ?> a) => a -> 'Noframes > a
noframes_ = Child

noscript_ :: ('Noscript ?> a) => a -> 'Noscript > a
noscript_ = Child

object_ :: ('Object ?> a) => a -> 'Object > a
object_ = Child

ol_ :: ('Ol ?> a) => a -> 'Ol > a
ol_ = Child

optgroup_ :: ('Optgroup ?> a) => a -> 'Optgroup > a
optgroup_ = Child

option_ :: ('Option ?> a) => a -> 'Option > a
option_ = Child

output_ :: ('Output ?> a) => a -> 'Output > a
output_ = Child

p_ :: ('P ?> a) => a -> 'P > a
p_ = Child

param_ :: 'Param > ()
param_ = Child ()

picture_ :: ('Picture ?> a) => a -> 'Picture > a
picture_ = Child

plaintext_ :: ('Plaintext ?> a) => a -> 'Plaintext > a
plaintext_ = Child

pre_ :: ('Pre ?> a) => a -> 'Pre > a
pre_ = Child

progress_ :: ('Progress ?> a) => a -> 'Progress > a
progress_ = Child

q_ :: ('Q ?> a) => a -> 'Q > a
q_ = Child

rp_ :: ('Rp ?> a) => a -> 'Rp > a
rp_ = Child

rt_ :: ('Rt ?> a) => a -> 'Rt > a
rt_ = Child

rtc_ :: ('Rtc ?> a) => a -> 'Rtc > a
rtc_ = Child

ruby_ :: ('Ruby ?> a) => a -> 'Ruby > a
ruby_ = Child

s_ :: ('S ?> a) => a -> 'S > a
s_ = Child

samp_ :: ('Samp ?> a) => a -> 'Samp > a
samp_ = Child

script_ :: ('Script ?> a) => a -> 'Script > a
script_ = Child

section_ :: ('Section ?> a) => a -> 'Section > a
section_ = Child

select_ :: ('Select ?> a) => a -> 'Select > a
select_ = Child

shadow_ :: ('Shadow ?> a) => a -> 'Shadow > a
shadow_ = Child

slot_ :: ('Slot ?> a) => a -> 'Slot > a
slot_ = Child

small_ :: ('Small ?> a) => a -> 'Small > a
small_ = Child

source_ :: 'Source > ()
source_ = Child ()

spacer_ :: ('Spacer ?> a) => a -> 'Spacer > a
spacer_ = Child

span_ :: ('Span ?> a) => a -> 'Span > a
span_ = Child

strike_ :: ('Strike ?> a) => a -> 'Strike > a
strike_ = Child

strong_ :: ('Strong ?> a) => a -> 'Strong > a
strong_ = Child

style_ :: ('Style ?> a) => a -> 'Style > a
style_ = Child

sub_ :: ('Sub ?> a) => a -> 'Sub > a
sub_ = Child

summary_ :: ('Summary ?> a) => a -> 'Summary > a
summary_ = Child

sup_ :: ('Sup ?> a) => a -> 'Sup > a
sup_ = Child

svg_ :: ('Svg ?> a) => a -> 'Svg > a
svg_ = Child

table_ :: ('Table ?> a) => a -> 'Table > a
table_ = Child

tbody_ :: ('Tbody ?> a) => a -> 'Tbody > a
tbody_ = Child

td_ :: ('Td ?> a) => a -> 'Td > a
td_ = Child

template_ :: ('Template ?> a) => a -> 'Template > a
template_ = Child

textarea_ :: ('Textarea ?> a) => a -> 'Textarea > a
textarea_ = Child

tfoot_ :: ('Tfoot ?> a) => a -> 'Tfoot > a
tfoot_ = Child

th_ :: ('Th ?> a) => a -> 'Th > a
th_ = Child

thead_ :: ('Thead ?> a) => a -> 'Thead > a
thead_ = Child

time_ :: ('Time ?> a) => a -> 'Time > a
time_ = Child

title_ :: ('Title ?> a) => a -> 'Title > a
title_ = Child

tr_ :: ('Tr ?> a) => a -> 'Tr > a
tr_ = Child

track_ :: 'Track > ()
track_ = Child ()

tt_ :: ('Tt ?> a) => a -> 'Tt > a
tt_ = Child

u_ :: ('U ?> a) => a -> 'U > a
u_ = Child

ul_ :: ('Ul ?> a) => a -> 'Ul > a
ul_ = Child

var_ :: ('Var ?> a) => a -> 'Var > a
var_ = Child

video_ :: ('Video ?> a) => a -> 'Video > a
video_ = Child

wbr_ :: 'Wbr > ()
wbr_ = Child ()

xmp_ :: ('Xmp ?> a) => a -> 'Xmp > a
xmp_ = Child
