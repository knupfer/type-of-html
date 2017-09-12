{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Element where

import Html.Type

doctype_ :: 'DOCTYPE > ()
doctype_ = Child ()

a_ :: ('A ?> a) => a -> 'A > a
a_ = Child

a_A :: ('A ??> a, 'A ?> b) => a -> b -> ('A :@: a) b
a_A = WithAttributes

abbr_ :: ('Abbr ?> a) => a -> 'Abbr > a
abbr_ = Child

abbr_A :: ('Abbr ??> a, 'Abbr ?> b) => a -> b -> ('Abbr :@: a) b
abbr_A = WithAttributes

acronym_ :: ('Acronym ?> a) => a -> 'Acronym > a
acronym_ = Child

acronym_A :: ('Acronym ??> a, 'Acronym ?> b) => a -> b -> ('Acronym :@: a) b
acronym_A = WithAttributes

address_ :: ('Address ?> a) => a -> 'Address > a
address_ = Child

address_A :: ('Address ??> a, 'Address ?> b) => a -> b -> ('Address :@: a) b
address_A = WithAttributes

applet_ :: ('Applet ?> a) => a -> 'Applet > a
applet_ = Child

applet_A :: ('Applet ??> a, 'Applet ?> b) => a -> b -> ('Applet :@: a) b
applet_A = WithAttributes

area_ :: 'Area > ()
area_ = Child ()

area_A :: 'Area ??> a => a -> ('Area :@: a) ()
area_A = flip WithAttributes ()

article_ :: ('Article ?> a) => a -> 'Article > a
article_ = Child

article_A :: ('Article ??> a, 'Article ?> b) => a -> b -> ('Article :@: a) b
article_A = WithAttributes

aside_ :: ('Aside ?> a) => a -> 'Aside > a
aside_ = Child

aside_A :: ('Aside ??> a, 'Aside ?> b) => a -> b -> ('Aside :@: a) b
aside_A = WithAttributes

audio_ :: ('Audio ?> a) => a -> 'Audio > a
audio_ = Child

audio_A :: ('Audio ??> a, 'Audio ?> b) => a -> b -> ('Audio :@: a) b
audio_A = WithAttributes

b_ :: ('B ?> a) => a -> 'B > a
b_ = Child

b_A :: ('B ??> a, 'B ?> b) => a -> b -> ('B :@: a) b
b_A = WithAttributes

base_ :: 'Base > ()
base_ = Child ()

base_A :: 'Base ??> a => a -> ('Base :@: a) ()
base_A = flip WithAttributes ()

basefont_ :: ('Basefont ?> a) => a -> 'Basefont > a
basefont_ = Child

basefont_A :: ('Basefont ??> a, 'Basefont ?> b) => a -> b -> ('Basefont :@: a) b
basefont_A = WithAttributes

bdi_ :: ('Bdi ?> a) => a -> 'Bdi > a
bdi_ = Child

bdi_A :: ('Bdi ??> a, 'Bdi ?> b) => a -> b -> ('Bdi :@: a) b
bdi_A = WithAttributes

bdo_ :: ('Bdo ?> a) => a -> 'Bdo > a
bdo_ = Child

bdo_A :: ('Bdo ??> a, 'Bdo ?> b) => a -> b -> ('Bdo :@: a) b
bdo_A = WithAttributes

bgsound_ :: ('Bgsound ?> a) => a -> 'Bgsound > a
bgsound_ = Child

bgsound_A :: ('Bgsound ??> a, 'Bgsound ?> b) => a -> b -> ('Bgsound :@: a) b
bgsound_A = WithAttributes

big_ :: ('Big ?> a) => a -> 'Big > a
big_ = Child

big_A :: ('Big ??> a, 'Big ?> b) => a -> b -> ('Big :@: a) b
big_A = WithAttributes

blink_ :: ('Blink ?> a) => a -> 'Blink > a
blink_ = Child

blink_A :: ('Blink ??> a, 'Blink ?> b) => a -> b -> ('Blink :@: a) b
blink_A = WithAttributes

blockquote_ :: ('Blockquote ?> a) => a -> 'Blockquote > a
blockquote_ = Child

blockquote_A :: ('Blockquote ??> a, 'Blockquote ?> b) => a -> b -> ('Blockquote :@: a) b
blockquote_A = WithAttributes

body_ :: ('Body ?> a) => a -> 'Body > a
body_ = Child

body_A :: ('Body ??> a, 'Body ?> b) => a -> b -> ('Body :@: a) b
body_A = WithAttributes

br_ :: 'Br > ()
br_ = Child ()

br_A :: 'Br ??> a => a -> ('Br :@: a) ()
br_A = flip WithAttributes ()

button_ :: ('Button ?> a) => a -> 'Button > a
button_ = Child

button_A :: ('Button ??> a, 'Button ?> b) => a -> b -> ('Button :@: a) b
button_A = WithAttributes

canvas_ :: ('Canvas ?> a) => a -> 'Canvas > a
canvas_ = Child

canvas_A :: ('Canvas ??> a, 'Canvas ?> b) => a -> b -> ('Canvas :@: a) b
canvas_A = WithAttributes

caption_ :: ('Caption ?> a) => a -> 'Caption > a
caption_ = Child

caption_A :: ('Caption ??> a, 'Caption ?> b) => a -> b -> ('Caption :@: a) b
caption_A = WithAttributes

center_ :: ('Center ?> a) => a -> 'Center > a
center_ = Child

center_A :: ('Center ??> a, 'Center ?> b) => a -> b -> ('Center :@: a) b
center_A = WithAttributes

cite_ :: ('Cite ?> a) => a -> 'Cite > a
cite_ = Child

cite_A :: ('Cite ??> a, 'Cite ?> b) => a -> b -> ('Cite :@: a) b
cite_A = WithAttributes

code_ :: ('Code ?> a) => a -> 'Code > a
code_ = Child

code_A :: ('Code ??> a, 'Code ?> b) => a -> b -> ('Code :@: a) b
code_A = WithAttributes

col_ :: 'Col > ()
col_ = Child ()

col_A :: 'Col ??> a => a -> ('Col :@: a) ()
col_A = flip WithAttributes ()

colgroup_ :: ('Colgroup ?> a) => a -> 'Colgroup > a
colgroup_ = Child

colgroup_A :: ('Colgroup ??> a, 'Colgroup ?> b) => a -> b -> ('Colgroup :@: a) b
colgroup_A = WithAttributes

command_ :: ('Command ?> a) => a -> 'Command > a
command_ = Child

command_A :: ('Command ??> a, 'Command ?> b) => a -> b -> ('Command :@: a) b
command_A = WithAttributes

content_ :: ('Content ?> a) => a -> 'Content > a
content_ = Child

content_A :: ('Content ??> a, 'Content ?> b) => a -> b -> ('Content :@: a) b
content_A = WithAttributes

data_ :: ('Data ?> a) => a -> 'Data > a
data_ = Child

data_A :: ('Data ??> a, 'Data ?> b) => a -> b -> ('Data :@: a) b
data_A = WithAttributes

datalist_ :: ('Datalist ?> a) => a -> 'Datalist > a
datalist_ = Child

datalist_A :: ('Datalist ??> a, 'Datalist ?> b) => a -> b -> ('Datalist :@: a) b
datalist_A = WithAttributes

dd_ :: ('Dd ?> a) => a -> 'Dd > a
dd_ = Child

dd_A :: ('Dd ??> a, 'Dd ?> b) => a -> b -> ('Dd :@: a) b
dd_A = WithAttributes

del_ :: ('Del ?> a) => a -> 'Del > a
del_ = Child

del_A :: ('Del ??> a, 'Del ?> b) => a -> b -> ('Del :@: a) b
del_A = WithAttributes

details_ :: ('Details ?> a) => a -> 'Details > a
details_ = Child

details_A :: ('Details ??> a, 'Details ?> b) => a -> b -> ('Details :@: a) b
details_A = WithAttributes

dfn_ :: ('Dfn ?> a) => a -> 'Dfn > a
dfn_ = Child

dfn_A :: ('Dfn ??> a, 'Dfn ?> b) => a -> b -> ('Dfn :@: a) b
dfn_A = WithAttributes

dialog_ :: ('Dialog ?> a) => a -> 'Dialog > a
dialog_ = Child

dialog_A :: ('Dialog ??> a, 'Dialog ?> b) => a -> b -> ('Dialog :@: a) b
dialog_A = WithAttributes

dir_ :: ('Dir ?> a) => a -> 'Dir > a
dir_ = Child

dir_A :: ('Dir ??> a, 'Dir ?> b) => a -> b -> ('Dir :@: a) b
dir_A = WithAttributes

div_ :: ('Div ?> a) => a -> 'Div > a
div_ = Child

div_A :: ('Div ??> a, 'Div ?> b) => a -> b -> ('Div :@: a) b
div_A = WithAttributes

dl_ :: ('Dl ?> a) => a -> 'Dl > a
dl_ = Child

dl_A :: ('Dl ??> a, 'Dl ?> b) => a -> b -> ('Dl :@: a) b
dl_A = WithAttributes

dt_ :: ('Dt ?> a) => a -> 'Dt > a
dt_ = Child

dt_A :: ('Dt ??> a, 'Dt ?> b) => a -> b -> ('Dt :@: a) b
dt_A = WithAttributes

element_ :: ('Element ?> a) => a -> 'Element > a
element_ = Child

element_A :: ('Element ??> a, 'Element ?> b) => a -> b -> ('Element :@: a) b
element_A = WithAttributes

em_ :: ('Em ?> a) => a -> 'Em > a
em_ = Child

em_A :: ('Em ??> a, 'Em ?> b) => a -> b -> ('Em :@: a) b
em_A = WithAttributes

embed_ :: 'Embed > ()
embed_ = Child ()

embed_A :: 'Embed ??> a => a -> ('Embed :@: a) ()
embed_A = flip WithAttributes ()

fieldset_ :: ('Fieldset ?> a) => a -> 'Fieldset > a
fieldset_ = Child

fieldset_A :: ('Fieldset ??> a, 'Fieldset ?> b) => a -> b -> ('Fieldset :@: a) b
fieldset_A = WithAttributes

figcaption_ :: ('Figcaption ?> a) => a -> 'Figcaption > a
figcaption_ = Child

figcaption_A :: ('Figcaption ??> a, 'Figcaption ?> b) => a -> b -> ('Figcaption :@: a) b
figcaption_A = WithAttributes

figure_ :: ('Figure ?> a) => a -> 'Figure > a
figure_ = Child

figure_A :: ('Figure ??> a, 'Figure ?> b) => a -> b -> ('Figure :@: a) b
figure_A = WithAttributes

font_ :: ('Font ?> a) => a -> 'Font > a
font_ = Child

font_A :: ('Font ??> a, 'Font ?> b) => a -> b -> ('Font :@: a) b
font_A = WithAttributes

footer_ :: ('Footer ?> a) => a -> 'Footer > a
footer_ = Child

footer_A :: ('Footer ??> a, 'Footer ?> b) => a -> b -> ('Footer :@: a) b
footer_A = WithAttributes

form_ :: ('Form ?> a) => a -> 'Form > a
form_ = Child

form_A :: ('Form ??> a, 'Form ?> b) => a -> b -> ('Form :@: a) b
form_A = WithAttributes

frame_ :: ('Frame ?> a) => a -> 'Frame > a
frame_ = Child

frame_A :: ('Frame ??> a, 'Frame ?> b) => a -> b -> ('Frame :@: a) b
frame_A = WithAttributes

frameset_ :: ('Frameset ?> a) => a -> 'Frameset > a
frameset_ = Child

frameset_A :: ('Frameset ??> a, 'Frameset ?> b) => a -> b -> ('Frameset :@: a) b
frameset_A = WithAttributes

h1_ :: ('H1 ?> a) => a -> 'H1 > a
h1_ = Child

h1_A :: ('H1 ??> a, 'H1 ?> b) => a -> b -> ('H1 :@: a) b
h1_A = WithAttributes

h2_ :: ('H2 ?> a) => a -> 'H2 > a
h2_ = Child

h2_A :: ('H2 ??> a, 'H2 ?> b) => a -> b -> ('H2 :@: a) b
h2_A = WithAttributes

h3_ :: ('H3 ?> a) => a -> 'H3 > a
h3_ = Child

h3_A :: ('H3 ??> a, 'H3 ?> b) => a -> b -> ('H3 :@: a) b
h3_A = WithAttributes

h4_ :: ('H4 ?> a) => a -> 'H4 > a
h4_ = Child

h4_A :: ('H4 ??> a, 'H4 ?> b) => a -> b -> ('H4 :@: a) b
h4_A = WithAttributes

h5_ :: ('H5 ?> a) => a -> 'H5 > a
h5_ = Child

h5_A :: ('H5 ??> a, 'H5 ?> b) => a -> b -> ('H5 :@: a) b
h5_A = WithAttributes

h6_ :: ('H6 ?> a) => a -> 'H6 > a
h6_ = Child

h6_A :: ('H6 ??> a, 'H6 ?> b) => a -> b -> ('H6 :@: a) b
h6_A = WithAttributes

head_ :: ('Head ?> a) => a -> 'Head > a
head_ = Child

head_A :: ('Head ??> a, 'Head ?> b) => a -> b -> ('Head :@: a) b
head_A = WithAttributes

header_ :: ('Header ?> a) => a -> 'Header > a
header_ = Child

header_A :: ('Header ??> a, 'Header ?> b) => a -> b -> ('Header :@: a) b
header_A = WithAttributes

hgroup_ :: ('Hgroup ?> a) => a -> 'Hgroup > a
hgroup_ = Child

hgroup_A :: ('Hgroup ??> a, 'Hgroup ?> b) => a -> b -> ('Hgroup :@: a) b
hgroup_A = WithAttributes

hr_ :: 'Hr > ()
hr_ = Child ()

hr_A :: 'Hr ??> a => a -> ('Hr :@: a) ()
hr_A = flip WithAttributes ()

html_ :: ('Html ?> a) => a -> 'Html > a
html_ = Child

html_A :: ('Html ??> a, 'Html ?> b) => a -> b -> ('Html :@: a) b
html_A = WithAttributes

i_ :: ('I ?> a) => a -> 'I > a
i_ = Child

i_A :: ('I ??> a, 'I ?> b) => a -> b -> ('I :@: a) b
i_A = WithAttributes

iframe_ :: 'Iframe > ()
iframe_ = Child ()

iframe_A :: 'Iframe ??> a => a -> ('Iframe :@: a) ()
iframe_A = flip WithAttributes ()

image_ :: ('Image ?> a) => a -> 'Image > a
image_ = Child

image_A :: ('Image ??> a, 'Image ?> b) => a -> b -> ('Image :@: a) b
image_A = WithAttributes

img_ :: 'Img > ()
img_ = Child ()

img_A :: 'Img ??> a => a -> ('Img :@: a) ()
img_A = flip WithAttributes ()

input_ :: ('Input ?> a) => a -> 'Input > a
input_ = Child

input_A :: ('Input ??> a, 'Input ?> b) => a -> b -> ('Input :@: a) b
input_A = WithAttributes

ins_ :: ('Ins ?> a) => a -> 'Ins > a
ins_ = Child

ins_A :: ('Ins ??> a, 'Ins ?> b) => a -> b -> ('Ins :@: a) b
ins_A = WithAttributes

isindex_ :: ('Isindex ?> a) => a -> 'Isindex > a
isindex_ = Child

isindex_A :: ('Isindex ??> a, 'Isindex ?> b) => a -> b -> ('Isindex :@: a) b
isindex_A = WithAttributes

kbd_ :: ('Kbd ?> a) => a -> 'Kbd > a
kbd_ = Child

kbd_A :: ('Kbd ??> a, 'Kbd ?> b) => a -> b -> ('Kbd :@: a) b
kbd_A = WithAttributes

keygen_ :: ('Keygen ?> a) => a -> 'Keygen > a
keygen_ = Child

keygen_A :: ('Keygen ??> a, 'Keygen ?> b) => a -> b -> ('Keygen :@: a) b
keygen_A = WithAttributes

label_ :: ('Label ?> a) => a -> 'Label > a
label_ = Child

label_A :: ('Label ??> a, 'Label ?> b) => a -> b -> ('Label :@: a) b
label_A = WithAttributes

legend_ :: ('Legend ?> a) => a -> 'Legend > a
legend_ = Child

legend_A :: ('Legend ??> a, 'Legend ?> b) => a -> b -> ('Legend :@: a) b
legend_A = WithAttributes

li_ :: ('Li ?> a) => a -> 'Li > a
li_ = Child

li_A :: ('Li ??> a, 'Li ?> b) => a -> b -> ('Li :@: a) b
li_A = WithAttributes

link_ :: 'Link > ()
link_ = Child ()

link_A :: 'Link ??> a => a -> ('Link :@: a) ()
link_A = flip WithAttributes ()

listing_ :: ('Listing ?> a) => a -> 'Listing > a
listing_ = Child

listing_A :: ('Listing ??> a, 'Listing ?> b) => a -> b -> ('Listing :@: a) b
listing_A = WithAttributes

main_ :: ('Main ?> a) => a -> 'Main > a
main_ = Child

main_A :: ('Main ??> a, 'Main ?> b) => a -> b -> ('Main :@: a) b
main_A = WithAttributes

map_ :: ('Map ?> a) => a -> 'Map > a
map_ = Child

map_A :: ('Map ??> a, 'Map ?> b) => a -> b -> ('Map :@: a) b
map_A = WithAttributes

mark_ :: ('Mark ?> a) => a -> 'Mark > a
mark_ = Child

mark_A :: ('Mark ??> a, 'Mark ?> b) => a -> b -> ('Mark :@: a) b
mark_A = WithAttributes

marquee_ :: ('Marquee ?> a) => a -> 'Marquee > a
marquee_ = Child

marquee_A :: ('Marquee ??> a, 'Marquee ?> b) => a -> b -> ('Marquee :@: a) b
marquee_A = WithAttributes

math_ :: ('Math ?> a) => a -> 'Math > a
math_ = Child

math_A :: ('Math ??> a, 'Math ?> b) => a -> b -> ('Math :@: a) b
math_A = WithAttributes

menu_ :: ('Menu ?> a) => a -> 'Menu > a
menu_ = Child

menu_A :: ('Menu ??> a, 'Menu ?> b) => a -> b -> ('Menu :@: a) b
menu_A = WithAttributes

menuitem_ :: 'Menuitem > ()
menuitem_ = Child ()

menuitem_A :: 'Menuitem ??> a => a -> ('Menuitem :@: a) ()
menuitem_A = flip WithAttributes ()

meta_ :: 'Meta > ()
meta_ = Child ()

meta_A :: 'Meta ??> a => a -> ('Meta :@: a) ()
meta_A = flip WithAttributes ()

meter_ :: ('Meter ?> a) => a -> 'Meter > a
meter_ = Child

meter_A :: ('Meter ??> a, 'Meter ?> b) => a -> b -> ('Meter :@: a) b
meter_A = WithAttributes

multicol_ :: ('Multicol ?> a) => a -> 'Multicol > a
multicol_ = Child

multicol_A :: ('Multicol ??> a, 'Multicol ?> b) => a -> b -> ('Multicol :@: a) b
multicol_A = WithAttributes

nav_ :: ('Nav ?> a) => a -> 'Nav > a
nav_ = Child

nav_A :: ('Nav ??> a, 'Nav ?> b) => a -> b -> ('Nav :@: a) b
nav_A = WithAttributes

nextid_ :: ('Nextid ?> a) => a -> 'Nextid > a
nextid_ = Child

nextid_A :: ('Nextid ??> a, 'Nextid ?> b) => a -> b -> ('Nextid :@: a) b
nextid_A = WithAttributes

nobr_ :: ('Nobr ?> a) => a -> 'Nobr > a
nobr_ = Child

nobr_A :: ('Nobr ??> a, 'Nobr ?> b) => a -> b -> ('Nobr :@: a) b
nobr_A = WithAttributes

noembed_ :: ('Noembed ?> a) => a -> 'Noembed > a
noembed_ = Child

noembed_A :: ('Noembed ??> a, 'Noembed ?> b) => a -> b -> ('Noembed :@: a) b
noembed_A = WithAttributes

noframes_ :: ('Noframes ?> a) => a -> 'Noframes > a
noframes_ = Child

noframes_A :: ('Noframes ??> a, 'Noframes ?> b) => a -> b -> ('Noframes :@: a) b
noframes_A = WithAttributes

noscript_ :: ('Noscript ?> a) => a -> 'Noscript > a
noscript_ = Child

noscript_A :: ('Noscript ??> a, 'Noscript ?> b) => a -> b -> ('Noscript :@: a) b
noscript_A = WithAttributes

object_ :: ('Object ?> a) => a -> 'Object > a
object_ = Child

object_A :: ('Object ??> a, 'Object ?> b) => a -> b -> ('Object :@: a) b
object_A = WithAttributes

ol_ :: ('Ol ?> a) => a -> 'Ol > a
ol_ = Child

ol_A :: ('Ol ??> a, 'Ol ?> b) => a -> b -> ('Ol :@: a) b
ol_A = WithAttributes

optgroup_ :: ('Optgroup ?> a) => a -> 'Optgroup > a
optgroup_ = Child

optgroup_A :: ('Optgroup ??> a, 'Optgroup ?> b) => a -> b -> ('Optgroup :@: a) b
optgroup_A = WithAttributes

option_ :: ('Option ?> a) => a -> 'Option > a
option_ = Child

option_A :: ('Option ??> a, 'Option ?> b) => a -> b -> ('Option :@: a) b
option_A = WithAttributes

output_ :: ('Output ?> a) => a -> 'Output > a
output_ = Child

output_A :: ('Output ??> a, 'Output ?> b) => a -> b -> ('Output :@: a) b
output_A = WithAttributes

p_ :: ('P ?> a) => a -> 'P > a
p_ = Child

p_A :: ('P ??> a, 'P ?> b) => a -> b -> ('P :@: a) b
p_A = WithAttributes

param_ :: 'Param > ()
param_ = Child ()

param_A :: 'Param ??> a => a -> ('Param :@: a) ()
param_A = flip WithAttributes ()

picture_ :: ('Picture ?> a) => a -> 'Picture > a
picture_ = Child

picture_A :: ('Picture ??> a, 'Picture ?> b) => a -> b -> ('Picture :@: a) b
picture_A = WithAttributes

plaintext_ :: ('Plaintext ?> a) => a -> 'Plaintext > a
plaintext_ = Child

plaintext_A :: ('Plaintext ??> a, 'Plaintext ?> b) => a -> b -> ('Plaintext :@: a) b
plaintext_A = WithAttributes

pre_ :: ('Pre ?> a) => a -> 'Pre > a
pre_ = Child

pre_A :: ('Pre ??> a, 'Pre ?> b) => a -> b -> ('Pre :@: a) b
pre_A = WithAttributes

progress_ :: ('Progress ?> a) => a -> 'Progress > a
progress_ = Child

progress_A :: ('Progress ??> a, 'Progress ?> b) => a -> b -> ('Progress :@: a) b
progress_A = WithAttributes

q_ :: ('Q ?> a) => a -> 'Q > a
q_ = Child

q_A :: ('Q ??> a, 'Q ?> b) => a -> b -> ('Q :@: a) b
q_A = WithAttributes

rp_ :: ('Rp ?> a) => a -> 'Rp > a
rp_ = Child

rp_A :: ('Rp ??> a, 'Rp ?> b) => a -> b -> ('Rp :@: a) b
rp_A = WithAttributes

rt_ :: ('Rt ?> a) => a -> 'Rt > a
rt_ = Child

rt_A :: ('Rt ??> a, 'Rt ?> b) => a -> b -> ('Rt :@: a) b
rt_A = WithAttributes

rtc_ :: ('Rtc ?> a) => a -> 'Rtc > a
rtc_ = Child

rtc_A :: ('Rtc ??> a, 'Rtc ?> b) => a -> b -> ('Rtc :@: a) b
rtc_A = WithAttributes

ruby_ :: ('Ruby ?> a) => a -> 'Ruby > a
ruby_ = Child

ruby_A :: ('Ruby ??> a, 'Ruby ?> b) => a -> b -> ('Ruby :@: a) b
ruby_A = WithAttributes

s_ :: ('S ?> a) => a -> 'S > a
s_ = Child

s_A :: ('S ??> a, 'S ?> b) => a -> b -> ('S :@: a) b
s_A = WithAttributes

samp_ :: ('Samp ?> a) => a -> 'Samp > a
samp_ = Child

samp_A :: ('Samp ??> a, 'Samp ?> b) => a -> b -> ('Samp :@: a) b
samp_A = WithAttributes

script_ :: ('Script ?> a) => a -> 'Script > a
script_ = Child

script_A :: ('Script ??> a, 'Script ?> b) => a -> b -> ('Script :@: a) b
script_A = WithAttributes

section_ :: ('Section ?> a) => a -> 'Section > a
section_ = Child

section_A :: ('Section ??> a, 'Section ?> b) => a -> b -> ('Section :@: a) b
section_A = WithAttributes

select_ :: ('Select ?> a) => a -> 'Select > a
select_ = Child

select_A :: ('Select ??> a, 'Select ?> b) => a -> b -> ('Select :@: a) b
select_A = WithAttributes

shadow_ :: ('Shadow ?> a) => a -> 'Shadow > a
shadow_ = Child

shadow_A :: ('Shadow ??> a, 'Shadow ?> b) => a -> b -> ('Shadow :@: a) b
shadow_A = WithAttributes

slot_ :: ('Slot ?> a) => a -> 'Slot > a
slot_ = Child

slot_A :: ('Slot ??> a, 'Slot ?> b) => a -> b -> ('Slot :@: a) b
slot_A = WithAttributes

small_ :: ('Small ?> a) => a -> 'Small > a
small_ = Child

small_A :: ('Small ??> a, 'Small ?> b) => a -> b -> ('Small :@: a) b
small_A = WithAttributes

source_ :: 'Source > ()
source_ = Child ()

source_A :: 'Source ??> a => a -> ('Source :@: a) ()
source_A = flip WithAttributes ()

spacer_ :: ('Spacer ?> a) => a -> 'Spacer > a
spacer_ = Child

spacer_A :: ('Spacer ??> a, 'Spacer ?> b) => a -> b -> ('Spacer :@: a) b
spacer_A = WithAttributes

span_ :: ('Span ?> a) => a -> 'Span > a
span_ = Child

span_A :: ('Span ??> a, 'Span ?> b) => a -> b -> ('Span :@: a) b
span_A = WithAttributes

strike_ :: ('Strike ?> a) => a -> 'Strike > a
strike_ = Child

strike_A :: ('Strike ??> a, 'Strike ?> b) => a -> b -> ('Strike :@: a) b
strike_A = WithAttributes

strong_ :: ('Strong ?> a) => a -> 'Strong > a
strong_ = Child

strong_A :: ('Strong ??> a, 'Strong ?> b) => a -> b -> ('Strong :@: a) b
strong_A = WithAttributes

style_ :: ('Style ?> a) => a -> 'Style > a
style_ = Child

style_A :: ('Style ??> a, 'Style ?> b) => a -> b -> ('Style :@: a) b
style_A = WithAttributes

sub_ :: ('Sub ?> a) => a -> 'Sub > a
sub_ = Child

sub_A :: ('Sub ??> a, 'Sub ?> b) => a -> b -> ('Sub :@: a) b
sub_A = WithAttributes

summary_ :: ('Summary ?> a) => a -> 'Summary > a
summary_ = Child

summary_A :: ('Summary ??> a, 'Summary ?> b) => a -> b -> ('Summary :@: a) b
summary_A = WithAttributes

sup_ :: ('Sup ?> a) => a -> 'Sup > a
sup_ = Child

sup_A :: ('Sup ??> a, 'Sup ?> b) => a -> b -> ('Sup :@: a) b
sup_A = WithAttributes

svg_ :: ('Svg ?> a) => a -> 'Svg > a
svg_ = Child

svg_A :: ('Svg ??> a, 'Svg ?> b) => a -> b -> ('Svg :@: a) b
svg_A = WithAttributes

table_ :: ('Table ?> a) => a -> 'Table > a
table_ = Child

table_A :: ('Table ??> a, 'Table ?> b) => a -> b -> ('Table :@: a) b
table_A = WithAttributes

tbody_ :: ('Tbody ?> a) => a -> 'Tbody > a
tbody_ = Child

tbody_A :: ('Tbody ??> a, 'Tbody ?> b) => a -> b -> ('Tbody :@: a) b
tbody_A = WithAttributes

td_ :: ('Td ?> a) => a -> 'Td > a
td_ = Child

td_A :: ('Td ??> a, 'Td ?> b) => a -> b -> ('Td :@: a) b
td_A = WithAttributes

template_ :: ('Template ?> a) => a -> 'Template > a
template_ = Child

template_A :: ('Template ??> a, 'Template ?> b) => a -> b -> ('Template :@: a) b
template_A = WithAttributes

textarea_ :: ('Textarea ?> a) => a -> 'Textarea > a
textarea_ = Child

textarea_A :: ('Textarea ??> a, 'Textarea ?> b) => a -> b -> ('Textarea :@: a) b
textarea_A = WithAttributes

tfoot_ :: ('Tfoot ?> a) => a -> 'Tfoot > a
tfoot_ = Child

tfoot_A :: ('Tfoot ??> a, 'Tfoot ?> b) => a -> b -> ('Tfoot :@: a) b
tfoot_A = WithAttributes

th_ :: ('Th ?> a) => a -> 'Th > a
th_ = Child

th_A :: ('Th ??> a, 'Th ?> b) => a -> b -> ('Th :@: a) b
th_A = WithAttributes

thead_ :: ('Thead ?> a) => a -> 'Thead > a
thead_ = Child

thead_A :: ('Thead ??> a, 'Thead ?> b) => a -> b -> ('Thead :@: a) b
thead_A = WithAttributes

time_ :: ('Time ?> a) => a -> 'Time > a
time_ = Child

time_A :: ('Time ??> a, 'Time ?> b) => a -> b -> ('Time :@: a) b
time_A = WithAttributes

title_ :: ('Title ?> a) => a -> 'Title > a
title_ = Child

title_A :: ('Title ??> a, 'Title ?> b) => a -> b -> ('Title :@: a) b
title_A = WithAttributes

tr_ :: ('Tr ?> a) => a -> 'Tr > a
tr_ = Child

tr_A :: ('Tr ??> a, 'Tr ?> b) => a -> b -> ('Tr :@: a) b
tr_A = WithAttributes

track_ :: 'Track > ()
track_ = Child ()

track_A :: 'Track ??> a => a -> ('Track :@: a) ()
track_A = flip WithAttributes ()

tt_ :: ('Tt ?> a) => a -> 'Tt > a
tt_ = Child

tt_A :: ('Tt ??> a, 'Tt ?> b) => a -> b -> ('Tt :@: a) b
tt_A = WithAttributes

u_ :: ('U ?> a) => a -> 'U > a
u_ = Child

u_A :: ('U ??> a, 'U ?> b) => a -> b -> ('U :@: a) b
u_A = WithAttributes

ul_ :: ('Ul ?> a) => a -> 'Ul > a
ul_ = Child

ul_A :: ('Ul ??> a, 'Ul ?> b) => a -> b -> ('Ul :@: a) b
ul_A = WithAttributes

var_ :: ('Var ?> a) => a -> 'Var > a
var_ = Child

var_A :: ('Var ??> a, 'Var ?> b) => a -> b -> ('Var :@: a) b
var_A = WithAttributes

video_ :: ('Video ?> a) => a -> 'Video > a
video_ = Child

video_A :: ('Video ??> a, 'Video ?> b) => a -> b -> ('Video :@: a) b
video_A = WithAttributes

wbr_ :: 'Wbr > ()
wbr_ = Child ()

wbr_A :: 'Wbr ??> a => a -> ('Wbr :@: a) ()
wbr_A = flip WithAttributes ()

xmp_ :: ('Xmp ?> a) => a -> 'Xmp > a
xmp_ = Child

xmp_A :: ('Xmp ??> a, 'Xmp ?> b) => a -> b -> ('Xmp :@: a) b
xmp_A = WithAttributes
