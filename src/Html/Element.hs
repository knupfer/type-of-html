{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Element where

import Html.Type

doctype_ :: 'DOCTYPE > ()
doctype_ = Child ()

a_ :: ('A ?> a) => a -> 'A > a
a_ = Child

a_A :: ('A ?> a) => Attribute -> a -> 'A :> a
a_A = WithAttributes

abbr_ :: ('Abbr ?> a) => a -> 'Abbr > a
abbr_ = Child

abbr_A :: ('Abbr ?> a) => Attribute -> a -> 'Abbr :> a
abbr_A = WithAttributes

acronym_ :: ('Acronym ?> a) => a -> 'Acronym > a
acronym_ = Child

acronym_A :: ('Acronym ?> a) => Attribute -> a -> 'Acronym :> a
acronym_A = WithAttributes

address_ :: ('Address ?> a) => a -> 'Address > a
address_ = Child

address_A :: ('Address ?> a) => Attribute -> a -> 'Address :> a
address_A = WithAttributes

applet_ :: ('Applet ?> a) => a -> 'Applet > a
applet_ = Child

applet_A :: ('Applet ?> a) => Attribute -> a -> 'Applet :> a
applet_A = WithAttributes

area_ :: 'Area > ()
area_ = Child ()

area_A :: Attribute -> 'Area :> ()
area_A = flip WithAttributes ()

article_ :: ('Article ?> a) => a -> 'Article > a
article_ = Child

article_A :: ('Article ?> a) => Attribute -> a -> 'Article :> a
article_A = WithAttributes

aside_ :: ('Aside ?> a) => a -> 'Aside > a
aside_ = Child

aside_A :: ('Aside ?> a) => Attribute -> a -> 'Aside :> a
aside_A = WithAttributes

audio_ :: ('Audio ?> a) => a -> 'Audio > a
audio_ = Child

audio_A :: ('Audio ?> a) => Attribute -> a -> 'Audio :> a
audio_A = WithAttributes

b_ :: ('B ?> a) => a -> 'B > a
b_ = Child

b_A :: ('B ?> a) => Attribute -> a -> 'B :> a
b_A = WithAttributes

base_ :: 'Base > ()
base_ = Child ()

base_A :: Attribute -> 'Base :> ()
base_A = flip WithAttributes ()

basefont_ :: ('Basefont ?> a) => a -> 'Basefont > a
basefont_ = Child

basefont_A :: ('Basefont ?> a) => Attribute -> a -> 'Basefont :> a
basefont_A = WithAttributes

bdi_ :: ('Bdi ?> a) => a -> 'Bdi > a
bdi_ = Child

bdi_A :: ('Bdi ?> a) => Attribute -> a -> 'Bdi :> a
bdi_A = WithAttributes

bdo_ :: ('Bdo ?> a) => a -> 'Bdo > a
bdo_ = Child

bdo_A :: ('Bdo ?> a) => Attribute -> a -> 'Bdo :> a
bdo_A = WithAttributes

bgsound_ :: ('Bgsound ?> a) => a -> 'Bgsound > a
bgsound_ = Child

bgsound_A :: ('Bgsound ?> a) => Attribute -> a -> 'Bgsound :> a
bgsound_A = WithAttributes

big_ :: ('Big ?> a) => a -> 'Big > a
big_ = Child

big_A :: ('Big ?> a) => Attribute -> a -> 'Big :> a
big_A = WithAttributes

blink_ :: ('Blink ?> a) => a -> 'Blink > a
blink_ = Child

blink_A :: ('Blink ?> a) => Attribute -> a -> 'Blink :> a
blink_A = WithAttributes

blockquote_ :: ('Blockquote ?> a) => a -> 'Blockquote > a
blockquote_ = Child

blockquote_A :: ('Blockquote ?> a) => Attribute -> a -> 'Blockquote :> a
blockquote_A = WithAttributes

body_ :: ('Body ?> a) => a -> 'Body > a
body_ = Child

body_A :: ('Body ?> a) => Attribute -> a -> 'Body :> a
body_A = WithAttributes

br_ :: 'Br > ()
br_ = Child ()

br_A :: Attribute -> 'Br :> ()
br_A = flip WithAttributes ()

button_ :: ('Button ?> a) => a -> 'Button > a
button_ = Child

button_A :: ('Button ?> a) => Attribute -> a -> 'Button :> a
button_A = WithAttributes

canvas_ :: ('Canvas ?> a) => a -> 'Canvas > a
canvas_ = Child

canvas_A :: ('Canvas ?> a) => Attribute -> a -> 'Canvas :> a
canvas_A = WithAttributes

caption_ :: ('Caption ?> a) => a -> 'Caption > a
caption_ = Child

caption_A :: ('Caption ?> a) => Attribute -> a -> 'Caption :> a
caption_A = WithAttributes

center_ :: ('Center ?> a) => a -> 'Center > a
center_ = Child

center_A :: ('Center ?> a) => Attribute -> a -> 'Center :> a
center_A = WithAttributes

cite_ :: ('Cite ?> a) => a -> 'Cite > a
cite_ = Child

cite_A :: ('Cite ?> a) => Attribute -> a -> 'Cite :> a
cite_A = WithAttributes

code_ :: ('Code ?> a) => a -> 'Code > a
code_ = Child

code_A :: ('Code ?> a) => Attribute -> a -> 'Code :> a
code_A = WithAttributes

col_ :: 'Col > ()
col_ = Child ()

col_A :: Attribute -> 'Col :> ()
col_A = flip WithAttributes ()

colgroup_ :: ('Colgroup ?> a) => a -> 'Colgroup > a
colgroup_ = Child

colgroup_A :: ('Colgroup ?> a) => Attribute -> a -> 'Colgroup :> a
colgroup_A = WithAttributes

command_ :: ('Command ?> a) => a -> 'Command > a
command_ = Child

command_A :: ('Command ?> a) => Attribute -> a -> 'Command :> a
command_A = WithAttributes

content_ :: ('Content ?> a) => a -> 'Content > a
content_ = Child

content_A :: ('Content ?> a) => Attribute -> a -> 'Content :> a
content_A = WithAttributes

data_ :: ('Data ?> a) => a -> 'Data > a
data_ = Child

data_A :: ('Data ?> a) => Attribute -> a -> 'Data :> a
data_A = WithAttributes

datalist_ :: ('Datalist ?> a) => a -> 'Datalist > a
datalist_ = Child

datalist_A :: ('Datalist ?> a) => Attribute -> a -> 'Datalist :> a
datalist_A = WithAttributes

dd_ :: ('Dd ?> a) => a -> 'Dd > a
dd_ = Child

dd_A :: ('Dd ?> a) => Attribute -> a -> 'Dd :> a
dd_A = WithAttributes

del_ :: ('Del ?> a) => a -> 'Del > a
del_ = Child

del_A :: ('Del ?> a) => Attribute -> a -> 'Del :> a
del_A = WithAttributes

details_ :: ('Details ?> a) => a -> 'Details > a
details_ = Child

details_A :: ('Details ?> a) => Attribute -> a -> 'Details :> a
details_A = WithAttributes

dfn_ :: ('Dfn ?> a) => a -> 'Dfn > a
dfn_ = Child

dfn_A :: ('Dfn ?> a) => Attribute -> a -> 'Dfn :> a
dfn_A = WithAttributes

dialog_ :: ('Dialog ?> a) => a -> 'Dialog > a
dialog_ = Child

dialog_A :: ('Dialog ?> a) => Attribute -> a -> 'Dialog :> a
dialog_A = WithAttributes

dir_ :: ('Dir ?> a) => a -> 'Dir > a
dir_ = Child

dir_A :: ('Dir ?> a) => Attribute -> a -> 'Dir :> a
dir_A = WithAttributes

div_ :: ('Div ?> a) => a -> 'Div > a
div_ = Child

div_A :: ('Div ?> a) => Attribute -> a -> 'Div :> a
div_A = WithAttributes

dl_ :: ('Dl ?> a) => a -> 'Dl > a
dl_ = Child

dl_A :: ('Dl ?> a) => Attribute -> a -> 'Dl :> a
dl_A = WithAttributes

dt_ :: ('Dt ?> a) => a -> 'Dt > a
dt_ = Child

dt_A :: ('Dt ?> a) => Attribute -> a -> 'Dt :> a
dt_A = WithAttributes

element_ :: ('Element ?> a) => a -> 'Element > a
element_ = Child

element_A :: ('Element ?> a) => Attribute -> a -> 'Element :> a
element_A = WithAttributes

em_ :: ('Em ?> a) => a -> 'Em > a
em_ = Child

em_A :: ('Em ?> a) => Attribute -> a -> 'Em :> a
em_A = WithAttributes

embed_ :: 'Embed > ()
embed_ = Child ()

embed_A :: Attribute -> 'Embed :> ()
embed_A = flip WithAttributes ()

fieldset_ :: ('Fieldset ?> a) => a -> 'Fieldset > a
fieldset_ = Child

fieldset_A :: ('Fieldset ?> a) => Attribute -> a -> 'Fieldset :> a
fieldset_A = WithAttributes

figcaption_ :: ('Figcaption ?> a) => a -> 'Figcaption > a
figcaption_ = Child

figcaption_A :: ('Figcaption ?> a) => Attribute -> a -> 'Figcaption :> a
figcaption_A = WithAttributes

figure_ :: ('Figure ?> a) => a -> 'Figure > a
figure_ = Child

figure_A :: ('Figure ?> a) => Attribute -> a -> 'Figure :> a
figure_A = WithAttributes

font_ :: ('Font ?> a) => a -> 'Font > a
font_ = Child

font_A :: ('Font ?> a) => Attribute -> a -> 'Font :> a
font_A = WithAttributes

footer_ :: ('Footer ?> a) => a -> 'Footer > a
footer_ = Child

footer_A :: ('Footer ?> a) => Attribute -> a -> 'Footer :> a
footer_A = WithAttributes

form_ :: ('Form ?> a) => a -> 'Form > a
form_ = Child

form_A :: ('Form ?> a) => Attribute -> a -> 'Form :> a
form_A = WithAttributes

frame_ :: ('Frame ?> a) => a -> 'Frame > a
frame_ = Child

frame_A :: ('Frame ?> a) => Attribute -> a -> 'Frame :> a
frame_A = WithAttributes

frameset_ :: ('Frameset ?> a) => a -> 'Frameset > a
frameset_ = Child

frameset_A :: ('Frameset ?> a) => Attribute -> a -> 'Frameset :> a
frameset_A = WithAttributes

h1_ :: ('H1 ?> a) => a -> 'H1 > a
h1_ = Child

h1_A :: ('H1 ?> a) => Attribute -> a -> 'H1 :> a
h1_A = WithAttributes

h2_ :: ('H2 ?> a) => a -> 'H2 > a
h2_ = Child

h2_A :: ('H2 ?> a) => Attribute -> a -> 'H2 :> a
h2_A = WithAttributes

h3_ :: ('H3 ?> a) => a -> 'H3 > a
h3_ = Child

h3_A :: ('H3 ?> a) => Attribute -> a -> 'H3 :> a
h3_A = WithAttributes

h4_ :: ('H4 ?> a) => a -> 'H4 > a
h4_ = Child

h4_A :: ('H4 ?> a) => Attribute -> a -> 'H4 :> a
h4_A = WithAttributes

h5_ :: ('H5 ?> a) => a -> 'H5 > a
h5_ = Child

h5_A :: ('H5 ?> a) => Attribute -> a -> 'H5 :> a
h5_A = WithAttributes

h6_ :: ('H6 ?> a) => a -> 'H6 > a
h6_ = Child

h6_A :: ('H6 ?> a) => Attribute -> a -> 'H6 :> a
h6_A = WithAttributes

head_ :: ('Head ?> a) => a -> 'Head > a
head_ = Child

head_A :: ('Head ?> a) => Attribute -> a -> 'Head :> a
head_A = WithAttributes

header_ :: ('Header ?> a) => a -> 'Header > a
header_ = Child

header_A :: ('Header ?> a) => Attribute -> a -> 'Header :> a
header_A = WithAttributes

hgroup_ :: ('Hgroup ?> a) => a -> 'Hgroup > a
hgroup_ = Child

hgroup_A :: ('Hgroup ?> a) => Attribute -> a -> 'Hgroup :> a
hgroup_A = WithAttributes

hr_ :: 'Hr > ()
hr_ = Child ()

hr_A :: Attribute -> 'Hr :> ()
hr_A = flip WithAttributes ()

html_ :: ('Html ?> a) => a -> 'Html > a
html_ = Child

html_A :: ('Html ?> a) => Attribute -> a -> 'Html :> a
html_A = WithAttributes

i_ :: ('I ?> a) => a -> 'I > a
i_ = Child

i_A :: ('I ?> a) => Attribute -> a -> 'I :> a
i_A = WithAttributes

iframe_ :: 'Iframe > ()
iframe_ = Child ()

iframe_A :: Attribute -> 'Iframe :> ()
iframe_A = flip WithAttributes ()

image_ :: ('Image ?> a) => a -> 'Image > a
image_ = Child

image_A :: ('Image ?> a) => Attribute -> a -> 'Image :> a
image_A = WithAttributes

img_ :: 'Img > ()
img_ = Child ()

img_A :: Attribute -> 'Img :> ()
img_A = flip WithAttributes ()

input_ :: ('Input ?> a) => a -> 'Input > a
input_ = Child

input_A :: ('Input ?> a) => Attribute -> a -> 'Input :> a
input_A = WithAttributes

ins_ :: ('Ins ?> a) => a -> 'Ins > a
ins_ = Child

ins_A :: ('Ins ?> a) => Attribute -> a -> 'Ins :> a
ins_A = WithAttributes

isindex_ :: ('Isindex ?> a) => a -> 'Isindex > a
isindex_ = Child

isindex_A :: ('Isindex ?> a) => Attribute -> a -> 'Isindex :> a
isindex_A = WithAttributes

kbd_ :: ('Kbd ?> a) => a -> 'Kbd > a
kbd_ = Child

kbd_A :: ('Kbd ?> a) => Attribute -> a -> 'Kbd :> a
kbd_A = WithAttributes

keygen_ :: ('Keygen ?> a) => a -> 'Keygen > a
keygen_ = Child

keygen_A :: ('Keygen ?> a) => Attribute -> a -> 'Keygen :> a
keygen_A = WithAttributes

label_ :: ('Label ?> a) => a -> 'Label > a
label_ = Child

label_A :: ('Label ?> a) => Attribute -> a -> 'Label :> a
label_A = WithAttributes

legend_ :: ('Legend ?> a) => a -> 'Legend > a
legend_ = Child

legend_A :: ('Legend ?> a) => Attribute -> a -> 'Legend :> a
legend_A = WithAttributes

li_ :: ('Li ?> a) => a -> 'Li > a
li_ = Child

li_A :: ('Li ?> a) => Attribute -> a -> 'Li :> a
li_A = WithAttributes

link_ :: 'Link > ()
link_ = Child ()

link_A :: Attribute -> 'Link :> ()
link_A = flip WithAttributes ()

listing_ :: ('Listing ?> a) => a -> 'Listing > a
listing_ = Child

listing_A :: ('Listing ?> a) => Attribute -> a -> 'Listing :> a
listing_A = WithAttributes

main_ :: ('Main ?> a) => a -> 'Main > a
main_ = Child

main_A :: ('Main ?> a) => Attribute -> a -> 'Main :> a
main_A = WithAttributes

map_ :: ('Map ?> a) => a -> 'Map > a
map_ = Child

map_A :: ('Map ?> a) => Attribute -> a -> 'Map :> a
map_A = WithAttributes

mark_ :: ('Mark ?> a) => a -> 'Mark > a
mark_ = Child

mark_A :: ('Mark ?> a) => Attribute -> a -> 'Mark :> a
mark_A = WithAttributes

marquee_ :: ('Marquee ?> a) => a -> 'Marquee > a
marquee_ = Child

marquee_A :: ('Marquee ?> a) => Attribute -> a -> 'Marquee :> a
marquee_A = WithAttributes

math_ :: ('Math ?> a) => a -> 'Math > a
math_ = Child

math_A :: ('Math ?> a) => Attribute -> a -> 'Math :> a
math_A = WithAttributes

menu_ :: ('Menu ?> a) => a -> 'Menu > a
menu_ = Child

menu_A :: ('Menu ?> a) => Attribute -> a -> 'Menu :> a
menu_A = WithAttributes

menuitem_ :: 'Menuitem > ()
menuitem_ = Child ()

menuitem_A :: Attribute -> 'Menuitem :> ()
menuitem_A = flip WithAttributes ()

meta_ :: 'Meta > ()
meta_ = Child ()

meta_A :: Attribute -> 'Meta :> ()
meta_A = flip WithAttributes ()

meter_ :: ('Meter ?> a) => a -> 'Meter > a
meter_ = Child

meter_A :: ('Meter ?> a) => Attribute -> a -> 'Meter :> a
meter_A = WithAttributes

multicol_ :: ('Multicol ?> a) => a -> 'Multicol > a
multicol_ = Child

multicol_A :: ('Multicol ?> a) => Attribute -> a -> 'Multicol :> a
multicol_A = WithAttributes

nav_ :: ('Nav ?> a) => a -> 'Nav > a
nav_ = Child

nav_A :: ('Nav ?> a) => Attribute -> a -> 'Nav :> a
nav_A = WithAttributes

nextid_ :: ('Nextid ?> a) => a -> 'Nextid > a
nextid_ = Child

nextid_A :: ('Nextid ?> a) => Attribute -> a -> 'Nextid :> a
nextid_A = WithAttributes

nobr_ :: ('Nobr ?> a) => a -> 'Nobr > a
nobr_ = Child

nobr_A :: ('Nobr ?> a) => Attribute -> a -> 'Nobr :> a
nobr_A = WithAttributes

noembed_ :: ('Noembed ?> a) => a -> 'Noembed > a
noembed_ = Child

noembed_A :: ('Noembed ?> a) => Attribute -> a -> 'Noembed :> a
noembed_A = WithAttributes

noframes_ :: ('Noframes ?> a) => a -> 'Noframes > a
noframes_ = Child

noframes_A :: ('Noframes ?> a) => Attribute -> a -> 'Noframes :> a
noframes_A = WithAttributes

noscript_ :: ('Noscript ?> a) => a -> 'Noscript > a
noscript_ = Child

noscript_A :: ('Noscript ?> a) => Attribute -> a -> 'Noscript :> a
noscript_A = WithAttributes

object_ :: ('Object ?> a) => a -> 'Object > a
object_ = Child

object_A :: ('Object ?> a) => Attribute -> a -> 'Object :> a
object_A = WithAttributes

ol_ :: ('Ol ?> a) => a -> 'Ol > a
ol_ = Child

ol_A :: ('Ol ?> a) => Attribute -> a -> 'Ol :> a
ol_A = WithAttributes

optgroup_ :: ('Optgroup ?> a) => a -> 'Optgroup > a
optgroup_ = Child

optgroup_A :: ('Optgroup ?> a) => Attribute -> a -> 'Optgroup :> a
optgroup_A = WithAttributes

option_ :: ('Option ?> a) => a -> 'Option > a
option_ = Child

option_A :: ('Option ?> a) => Attribute -> a -> 'Option :> a
option_A = WithAttributes

output_ :: ('Output ?> a) => a -> 'Output > a
output_ = Child

output_A :: ('Output ?> a) => Attribute -> a -> 'Output :> a
output_A = WithAttributes

p_ :: ('P ?> a) => a -> 'P > a
p_ = Child

p_A :: ('P ?> a) => Attribute -> a -> 'P :> a
p_A = WithAttributes

param_ :: 'Param > ()
param_ = Child ()

param_A :: Attribute -> 'Param :> ()
param_A = flip WithAttributes ()

picture_ :: ('Picture ?> a) => a -> 'Picture > a
picture_ = Child

picture_A :: ('Picture ?> a) => Attribute -> a -> 'Picture :> a
picture_A = WithAttributes

plaintext_ :: ('Plaintext ?> a) => a -> 'Plaintext > a
plaintext_ = Child

plaintext_A :: ('Plaintext ?> a) => Attribute -> a -> 'Plaintext :> a
plaintext_A = WithAttributes

pre_ :: ('Pre ?> a) => a -> 'Pre > a
pre_ = Child

pre_A :: ('Pre ?> a) => Attribute -> a -> 'Pre :> a
pre_A = WithAttributes

progress_ :: ('Progress ?> a) => a -> 'Progress > a
progress_ = Child

progress_A :: ('Progress ?> a) => Attribute -> a -> 'Progress :> a
progress_A = WithAttributes

q_ :: ('Q ?> a) => a -> 'Q > a
q_ = Child

q_A :: ('Q ?> a) => Attribute -> a -> 'Q :> a
q_A = WithAttributes

rp_ :: ('Rp ?> a) => a -> 'Rp > a
rp_ = Child

rp_A :: ('Rp ?> a) => Attribute -> a -> 'Rp :> a
rp_A = WithAttributes

rt_ :: ('Rt ?> a) => a -> 'Rt > a
rt_ = Child

rt_A :: ('Rt ?> a) => Attribute -> a -> 'Rt :> a
rt_A = WithAttributes

rtc_ :: ('Rtc ?> a) => a -> 'Rtc > a
rtc_ = Child

rtc_A :: ('Rtc ?> a) => Attribute -> a -> 'Rtc :> a
rtc_A = WithAttributes

ruby_ :: ('Ruby ?> a) => a -> 'Ruby > a
ruby_ = Child

ruby_A :: ('Ruby ?> a) => Attribute -> a -> 'Ruby :> a
ruby_A = WithAttributes

s_ :: ('S ?> a) => a -> 'S > a
s_ = Child

s_A :: ('S ?> a) => Attribute -> a -> 'S :> a
s_A = WithAttributes

samp_ :: ('Samp ?> a) => a -> 'Samp > a
samp_ = Child

samp_A :: ('Samp ?> a) => Attribute -> a -> 'Samp :> a
samp_A = WithAttributes

script_ :: ('Script ?> a) => a -> 'Script > a
script_ = Child

script_A :: ('Script ?> a) => Attribute -> a -> 'Script :> a
script_A = WithAttributes

section_ :: ('Section ?> a) => a -> 'Section > a
section_ = Child

section_A :: ('Section ?> a) => Attribute -> a -> 'Section :> a
section_A = WithAttributes

select_ :: ('Select ?> a) => a -> 'Select > a
select_ = Child

select_A :: ('Select ?> a) => Attribute -> a -> 'Select :> a
select_A = WithAttributes

shadow_ :: ('Shadow ?> a) => a -> 'Shadow > a
shadow_ = Child

shadow_A :: ('Shadow ?> a) => Attribute -> a -> 'Shadow :> a
shadow_A = WithAttributes

slot_ :: ('Slot ?> a) => a -> 'Slot > a
slot_ = Child

slot_A :: ('Slot ?> a) => Attribute -> a -> 'Slot :> a
slot_A = WithAttributes

small_ :: ('Small ?> a) => a -> 'Small > a
small_ = Child

small_A :: ('Small ?> a) => Attribute -> a -> 'Small :> a
small_A = WithAttributes

source_ :: 'Source > ()
source_ = Child ()

source_A :: Attribute -> 'Source :> ()
source_A = flip WithAttributes ()

spacer_ :: ('Spacer ?> a) => a -> 'Spacer > a
spacer_ = Child

spacer_A :: ('Spacer ?> a) => Attribute -> a -> 'Spacer :> a
spacer_A = WithAttributes

span_ :: ('Span ?> a) => a -> 'Span > a
span_ = Child

span_A :: ('Span ?> a) => Attribute -> a -> 'Span :> a
span_A = WithAttributes

strike_ :: ('Strike ?> a) => a -> 'Strike > a
strike_ = Child

strike_A :: ('Strike ?> a) => Attribute -> a -> 'Strike :> a
strike_A = WithAttributes

strong_ :: ('Strong ?> a) => a -> 'Strong > a
strong_ = Child

strong_A :: ('Strong ?> a) => Attribute -> a -> 'Strong :> a
strong_A = WithAttributes

style_ :: ('Style ?> a) => a -> 'Style > a
style_ = Child

style_A :: ('Style ?> a) => Attribute -> a -> 'Style :> a
style_A = WithAttributes

sub_ :: ('Sub ?> a) => a -> 'Sub > a
sub_ = Child

sub_A :: ('Sub ?> a) => Attribute -> a -> 'Sub :> a
sub_A = WithAttributes

summary_ :: ('Summary ?> a) => a -> 'Summary > a
summary_ = Child

summary_A :: ('Summary ?> a) => Attribute -> a -> 'Summary :> a
summary_A = WithAttributes

sup_ :: ('Sup ?> a) => a -> 'Sup > a
sup_ = Child

sup_A :: ('Sup ?> a) => Attribute -> a -> 'Sup :> a
sup_A = WithAttributes

svg_ :: ('Svg ?> a) => a -> 'Svg > a
svg_ = Child

svg_A :: ('Svg ?> a) => Attribute -> a -> 'Svg :> a
svg_A = WithAttributes

table_ :: ('Table ?> a) => a -> 'Table > a
table_ = Child

table_A :: ('Table ?> a) => Attribute -> a -> 'Table :> a
table_A = WithAttributes

tbody_ :: ('Tbody ?> a) => a -> 'Tbody > a
tbody_ = Child

tbody_A :: ('Tbody ?> a) => Attribute -> a -> 'Tbody :> a
tbody_A = WithAttributes

td_ :: ('Td ?> a) => a -> 'Td > a
td_ = Child

td_A :: ('Td ?> a) => Attribute -> a -> 'Td :> a
td_A = WithAttributes

template_ :: ('Template ?> a) => a -> 'Template > a
template_ = Child

template_A :: ('Template ?> a) => Attribute -> a -> 'Template :> a
template_A = WithAttributes

textarea_ :: ('Textarea ?> a) => a -> 'Textarea > a
textarea_ = Child

textarea_A :: ('Textarea ?> a) => Attribute -> a -> 'Textarea :> a
textarea_A = WithAttributes

tfoot_ :: ('Tfoot ?> a) => a -> 'Tfoot > a
tfoot_ = Child

tfoot_A :: ('Tfoot ?> a) => Attribute -> a -> 'Tfoot :> a
tfoot_A = WithAttributes

th_ :: ('Th ?> a) => a -> 'Th > a
th_ = Child

th_A :: ('Th ?> a) => Attribute -> a -> 'Th :> a
th_A = WithAttributes

thead_ :: ('Thead ?> a) => a -> 'Thead > a
thead_ = Child

thead_A :: ('Thead ?> a) => Attribute -> a -> 'Thead :> a
thead_A = WithAttributes

time_ :: ('Time ?> a) => a -> 'Time > a
time_ = Child

time_A :: ('Time ?> a) => Attribute -> a -> 'Time :> a
time_A = WithAttributes

title_ :: ('Title ?> a) => a -> 'Title > a
title_ = Child

title_A :: ('Title ?> a) => Attribute -> a -> 'Title :> a
title_A = WithAttributes

tr_ :: ('Tr ?> a) => a -> 'Tr > a
tr_ = Child

tr_A :: ('Tr ?> a) => Attribute -> a -> 'Tr :> a
tr_A = WithAttributes

track_ :: 'Track > ()
track_ = Child ()

track_A :: Attribute -> 'Track :> ()
track_A = flip WithAttributes ()

tt_ :: ('Tt ?> a) => a -> 'Tt > a
tt_ = Child

tt_A :: ('Tt ?> a) => Attribute -> a -> 'Tt :> a
tt_A = WithAttributes

u_ :: ('U ?> a) => a -> 'U > a
u_ = Child

u_A :: ('U ?> a) => Attribute -> a -> 'U :> a
u_A = WithAttributes

ul_ :: ('Ul ?> a) => a -> 'Ul > a
ul_ = Child

ul_A :: ('Ul ?> a) => Attribute -> a -> 'Ul :> a
ul_A = WithAttributes

var_ :: ('Var ?> a) => a -> 'Var > a
var_ = Child

var_A :: ('Var ?> a) => Attribute -> a -> 'Var :> a
var_A = WithAttributes

video_ :: ('Video ?> a) => a -> 'Video > a
video_ = Child

video_A :: ('Video ?> a) => Attribute -> a -> 'Video :> a
video_A = WithAttributes

wbr_ :: 'Wbr > ()
wbr_ = Child ()

wbr_A :: Attribute -> 'Wbr :> ()
wbr_A = flip WithAttributes ()

xmp_ :: ('Xmp ?> a) => a -> 'Xmp > a
xmp_ = Child

xmp_A :: ('Xmp ?> a) => Attribute -> a -> 'Xmp :> a
xmp_A = WithAttributes
