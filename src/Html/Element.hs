{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Element where

import Html.Type
import Html.Function (addAttributes)

doctype_ :: 'DOCTYPE > ()
doctype_ = Child ()

a_ :: ('A ?> a) => a -> 'A > a
a_ = Child

a_A :: ('A ?> a) => [(String, String)] -> a -> 'A :> a
a_A xs = addAttributes xs . Child

abbr_ :: ('Abbr ?> a) => a -> 'Abbr > a
abbr_ = Child

abbr_A :: ('Abbr ?> a) => [(String, String)] -> a -> 'Abbr :> a
abbr_A xs = addAttributes xs . Child

acronym_ :: ('Acronym ?> a) => a -> 'Acronym > a
acronym_ = Child

acronym_A :: ('Acronym ?> a) => [(String, String)] -> a -> 'Acronym :> a
acronym_A xs = addAttributes xs . Child

address_ :: ('Address ?> a) => a -> 'Address > a
address_ = Child

address_A :: ('Address ?> a) => [(String, String)] -> a -> 'Address :> a
address_A xs = addAttributes xs . Child

applet_ :: ('Applet ?> a) => a -> 'Applet > a
applet_ = Child

applet_A :: ('Applet ?> a) => [(String, String)] -> a -> 'Applet :> a
applet_A xs = addAttributes xs . Child

area_ :: 'Area > ()
area_ = Child ()

area_A :: [(String, String)] -> 'Area :> ()
area_A xs = addAttributes xs $ Child ()

article_ :: ('Article ?> a) => a -> 'Article > a
article_ = Child

article_A :: ('Article ?> a) => [(String, String)] -> a -> 'Article :> a
article_A xs = addAttributes xs . Child

aside_ :: ('Aside ?> a) => a -> 'Aside > a
aside_ = Child

aside_A :: ('Aside ?> a) => [(String, String)] -> a -> 'Aside :> a
aside_A xs = addAttributes xs . Child

audio_ :: ('Audio ?> a) => a -> 'Audio > a
audio_ = Child

audio_A :: ('Audio ?> a) => [(String, String)] -> a -> 'Audio :> a
audio_A xs = addAttributes xs . Child

b_ :: ('B ?> a) => a -> 'B > a
b_ = Child

b_A :: ('B ?> a) => [(String, String)] -> a -> 'B :> a
b_A xs = addAttributes xs . Child

base_ :: 'Base > ()
base_ = Child ()

base_A :: [(String, String)] -> 'Base :> ()
base_A xs = addAttributes xs $ Child ()

basefont_ :: ('Basefont ?> a) => a -> 'Basefont > a
basefont_ = Child

basefont_A :: ('Basefont ?> a) => [(String, String)] -> a -> 'Basefont :> a
basefont_A xs = addAttributes xs . Child

bdi_ :: ('Bdi ?> a) => a -> 'Bdi > a
bdi_ = Child

bdi_A :: ('Bdi ?> a) => [(String, String)] -> a -> 'Bdi :> a
bdi_A xs = addAttributes xs . Child

bdo_ :: ('Bdo ?> a) => a -> 'Bdo > a
bdo_ = Child

bdo_A :: ('Bdo ?> a) => [(String, String)] -> a -> 'Bdo :> a
bdo_A xs = addAttributes xs . Child

bgsound_ :: ('Bgsound ?> a) => a -> 'Bgsound > a
bgsound_ = Child

bgsound_A :: ('Bgsound ?> a) => [(String, String)] -> a -> 'Bgsound :> a
bgsound_A xs = addAttributes xs . Child

big_ :: ('Big ?> a) => a -> 'Big > a
big_ = Child

big_A :: ('Big ?> a) => [(String, String)] -> a -> 'Big :> a
big_A xs = addAttributes xs . Child

blink_ :: ('Blink ?> a) => a -> 'Blink > a
blink_ = Child

blink_A :: ('Blink ?> a) => [(String, String)] -> a -> 'Blink :> a
blink_A xs = addAttributes xs . Child

blockquote_ :: ('Blockquote ?> a) => a -> 'Blockquote > a
blockquote_ = Child

blockquote_A :: ('Blockquote ?> a) => [(String, String)] -> a -> 'Blockquote :> a
blockquote_A xs = addAttributes xs . Child

body_ :: ('Body ?> a) => a -> 'Body > a
body_ = Child

body_A :: ('Body ?> a) => [(String, String)] -> a -> 'Body :> a
body_A xs = addAttributes xs . Child

br_ :: 'Br > ()
br_ = Child ()

br_A :: [(String, String)] -> 'Br :> ()
br_A xs = addAttributes xs $ Child ()

button_ :: ('Button ?> a) => a -> 'Button > a
button_ = Child

button_A :: ('Button ?> a) => [(String, String)] -> a -> 'Button :> a
button_A xs = addAttributes xs . Child

canvas_ :: ('Canvas ?> a) => a -> 'Canvas > a
canvas_ = Child

canvas_A :: ('Canvas ?> a) => [(String, String)] -> a -> 'Canvas :> a
canvas_A xs = addAttributes xs . Child

caption_ :: ('Caption ?> a) => a -> 'Caption > a
caption_ = Child

caption_A :: ('Caption ?> a) => [(String, String)] -> a -> 'Caption :> a
caption_A xs = addAttributes xs . Child

center_ :: ('Center ?> a) => a -> 'Center > a
center_ = Child

center_A :: ('Center ?> a) => [(String, String)] -> a -> 'Center :> a
center_A xs = addAttributes xs . Child

cite_ :: ('Cite ?> a) => a -> 'Cite > a
cite_ = Child

cite_A :: ('Cite ?> a) => [(String, String)] -> a -> 'Cite :> a
cite_A xs = addAttributes xs . Child

code_ :: ('Code ?> a) => a -> 'Code > a
code_ = Child

code_A :: ('Code ?> a) => [(String, String)] -> a -> 'Code :> a
code_A xs = addAttributes xs . Child

col_ :: 'Col > ()
col_ = Child ()

col_A :: [(String, String)] -> 'Col :> ()
col_A xs = addAttributes xs $ Child ()

colgroup_ :: ('Colgroup ?> a) => a -> 'Colgroup > a
colgroup_ = Child

colgroup_A :: ('Colgroup ?> a) => [(String, String)] -> a -> 'Colgroup :> a
colgroup_A xs = addAttributes xs . Child

command_ :: ('Command ?> a) => a -> 'Command > a
command_ = Child

command_A :: ('Command ?> a) => [(String, String)] -> a -> 'Command :> a
command_A xs = addAttributes xs . Child

content_ :: ('Content ?> a) => a -> 'Content > a
content_ = Child

content_A :: ('Content ?> a) => [(String, String)] -> a -> 'Content :> a
content_A xs = addAttributes xs . Child

data_ :: ('Data ?> a) => a -> 'Data > a
data_ = Child

data_A :: ('Data ?> a) => [(String, String)] -> a -> 'Data :> a
data_A xs = addAttributes xs . Child

datalist_ :: ('Datalist ?> a) => a -> 'Datalist > a
datalist_ = Child

datalist_A :: ('Datalist ?> a) => [(String, String)] -> a -> 'Datalist :> a
datalist_A xs = addAttributes xs . Child

dd_ :: ('Dd ?> a) => a -> 'Dd > a
dd_ = Child

dd_A :: ('Dd ?> a) => [(String, String)] -> a -> 'Dd :> a
dd_A xs = addAttributes xs . Child

del_ :: ('Del ?> a) => a -> 'Del > a
del_ = Child

del_A :: ('Del ?> a) => [(String, String)] -> a -> 'Del :> a
del_A xs = addAttributes xs . Child

details_ :: ('Details ?> a) => a -> 'Details > a
details_ = Child

details_A :: ('Details ?> a) => [(String, String)] -> a -> 'Details :> a
details_A xs = addAttributes xs . Child

dfn_ :: ('Dfn ?> a) => a -> 'Dfn > a
dfn_ = Child

dfn_A :: ('Dfn ?> a) => [(String, String)] -> a -> 'Dfn :> a
dfn_A xs = addAttributes xs . Child

dialog_ :: ('Dialog ?> a) => a -> 'Dialog > a
dialog_ = Child

dialog_A :: ('Dialog ?> a) => [(String, String)] -> a -> 'Dialog :> a
dialog_A xs = addAttributes xs . Child

dir_ :: ('Dir ?> a) => a -> 'Dir > a
dir_ = Child

dir_A :: ('Dir ?> a) => [(String, String)] -> a -> 'Dir :> a
dir_A xs = addAttributes xs . Child

div_ :: ('Div ?> a) => a -> 'Div > a
div_ = Child

div_A :: ('Div ?> a) => [(String, String)] -> a -> 'Div :> a
div_A xs = addAttributes xs . Child

dl_ :: ('Dl ?> a) => a -> 'Dl > a
dl_ = Child

dl_A :: ('Dl ?> a) => [(String, String)] -> a -> 'Dl :> a
dl_A xs = addAttributes xs . Child

dt_ :: ('Dt ?> a) => a -> 'Dt > a
dt_ = Child

dt_A :: ('Dt ?> a) => [(String, String)] -> a -> 'Dt :> a
dt_A xs = addAttributes xs . Child

element_ :: ('Element ?> a) => a -> 'Element > a
element_ = Child

element_A :: ('Element ?> a) => [(String, String)] -> a -> 'Element :> a
element_A xs = addAttributes xs . Child

em_ :: ('Em ?> a) => a -> 'Em > a
em_ = Child

em_A :: ('Em ?> a) => [(String, String)] -> a -> 'Em :> a
em_A xs = addAttributes xs . Child

embed_ :: 'Embed > ()
embed_ = Child ()

embed_A :: [(String, String)] -> 'Embed :> ()
embed_A xs = addAttributes xs $ Child ()

fieldset_ :: ('Fieldset ?> a) => a -> 'Fieldset > a
fieldset_ = Child

fieldset_A :: ('Fieldset ?> a) => [(String, String)] -> a -> 'Fieldset :> a
fieldset_A xs = addAttributes xs . Child

figcaption_ :: ('Figcaption ?> a) => a -> 'Figcaption > a
figcaption_ = Child

figcaption_A :: ('Figcaption ?> a) => [(String, String)] -> a -> 'Figcaption :> a
figcaption_A xs = addAttributes xs . Child

figure_ :: ('Figure ?> a) => a -> 'Figure > a
figure_ = Child

figure_A :: ('Figure ?> a) => [(String, String)] -> a -> 'Figure :> a
figure_A xs = addAttributes xs . Child

font_ :: ('Font ?> a) => a -> 'Font > a
font_ = Child

font_A :: ('Font ?> a) => [(String, String)] -> a -> 'Font :> a
font_A xs = addAttributes xs . Child

footer_ :: ('Footer ?> a) => a -> 'Footer > a
footer_ = Child

footer_A :: ('Footer ?> a) => [(String, String)] -> a -> 'Footer :> a
footer_A xs = addAttributes xs . Child

form_ :: ('Form ?> a) => a -> 'Form > a
form_ = Child

form_A :: ('Form ?> a) => [(String, String)] -> a -> 'Form :> a
form_A xs = addAttributes xs . Child

frame_ :: ('Frame ?> a) => a -> 'Frame > a
frame_ = Child

frame_A :: ('Frame ?> a) => [(String, String)] -> a -> 'Frame :> a
frame_A xs = addAttributes xs . Child

frameset_ :: ('Frameset ?> a) => a -> 'Frameset > a
frameset_ = Child

frameset_A :: ('Frameset ?> a) => [(String, String)] -> a -> 'Frameset :> a
frameset_A xs = addAttributes xs . Child

h1_ :: ('H1 ?> a) => a -> 'H1 > a
h1_ = Child

h1_A :: ('H1 ?> a) => [(String, String)] -> a -> 'H1 :> a
h1_A xs = addAttributes xs . Child

h2_ :: ('H2 ?> a) => a -> 'H2 > a
h2_ = Child

h2_A :: ('H2 ?> a) => [(String, String)] -> a -> 'H2 :> a
h2_A xs = addAttributes xs . Child

h3_ :: ('H3 ?> a) => a -> 'H3 > a
h3_ = Child

h3_A :: ('H3 ?> a) => [(String, String)] -> a -> 'H3 :> a
h3_A xs = addAttributes xs . Child

h4_ :: ('H4 ?> a) => a -> 'H4 > a
h4_ = Child

h4_A :: ('H4 ?> a) => [(String, String)] -> a -> 'H4 :> a
h4_A xs = addAttributes xs . Child

h5_ :: ('H5 ?> a) => a -> 'H5 > a
h5_ = Child

h5_A :: ('H5 ?> a) => [(String, String)] -> a -> 'H5 :> a
h5_A xs = addAttributes xs . Child

h6_ :: ('H6 ?> a) => a -> 'H6 > a
h6_ = Child

h6_A :: ('H6 ?> a) => [(String, String)] -> a -> 'H6 :> a
h6_A xs = addAttributes xs . Child

head_ :: ('Head ?> a) => a -> 'Head > a
head_ = Child

head_A :: ('Head ?> a) => [(String, String)] -> a -> 'Head :> a
head_A xs = addAttributes xs . Child

header_ :: ('Header ?> a) => a -> 'Header > a
header_ = Child

header_A :: ('Header ?> a) => [(String, String)] -> a -> 'Header :> a
header_A xs = addAttributes xs . Child

hgroup_ :: ('Hgroup ?> a) => a -> 'Hgroup > a
hgroup_ = Child

hgroup_A :: ('Hgroup ?> a) => [(String, String)] -> a -> 'Hgroup :> a
hgroup_A xs = addAttributes xs . Child

hr_ :: 'Hr > ()
hr_ = Child ()

hr_A :: [(String, String)] -> 'Hr :> ()
hr_A xs = addAttributes xs $ Child ()

html_ :: ('Html ?> a) => a -> 'Html > a
html_ = Child

html_A :: ('Html ?> a) => [(String, String)] -> a -> 'Html :> a
html_A xs = addAttributes xs . Child

i_ :: ('I ?> a) => a -> 'I > a
i_ = Child

i_A :: ('I ?> a) => [(String, String)] -> a -> 'I :> a
i_A xs = addAttributes xs . Child

iframe_ :: 'Iframe > ()
iframe_ = Child ()

iframe_A :: [(String, String)] -> 'Iframe :> ()
iframe_A xs = addAttributes xs $ Child ()

image_ :: ('Image ?> a) => a -> 'Image > a
image_ = Child

image_A :: ('Image ?> a) => [(String, String)] -> a -> 'Image :> a
image_A xs = addAttributes xs . Child

img_ :: 'Img > ()
img_ = Child ()

img_A :: [(String, String)] -> 'Img :> ()
img_A xs = addAttributes xs $ Child ()

input_ :: ('Input ?> a) => a -> 'Input > a
input_ = Child

input_A :: ('Input ?> a) => [(String, String)] -> a -> 'Input :> a
input_A xs = addAttributes xs . Child

ins_ :: ('Ins ?> a) => a -> 'Ins > a
ins_ = Child

ins_A :: ('Ins ?> a) => [(String, String)] -> a -> 'Ins :> a
ins_A xs = addAttributes xs . Child

isindex_ :: ('Isindex ?> a) => a -> 'Isindex > a
isindex_ = Child

isindex_A :: ('Isindex ?> a) => [(String, String)] -> a -> 'Isindex :> a
isindex_A xs = addAttributes xs . Child

kbd_ :: ('Kbd ?> a) => a -> 'Kbd > a
kbd_ = Child

kbd_A :: ('Kbd ?> a) => [(String, String)] -> a -> 'Kbd :> a
kbd_A xs = addAttributes xs . Child

keygen_ :: ('Keygen ?> a) => a -> 'Keygen > a
keygen_ = Child

keygen_A :: ('Keygen ?> a) => [(String, String)] -> a -> 'Keygen :> a
keygen_A xs = addAttributes xs . Child

label_ :: ('Label ?> a) => a -> 'Label > a
label_ = Child

label_A :: ('Label ?> a) => [(String, String)] -> a -> 'Label :> a
label_A xs = addAttributes xs . Child

legend_ :: ('Legend ?> a) => a -> 'Legend > a
legend_ = Child

legend_A :: ('Legend ?> a) => [(String, String)] -> a -> 'Legend :> a
legend_A xs = addAttributes xs . Child

li_ :: ('Li ?> a) => a -> 'Li > a
li_ = Child

li_A :: ('Li ?> a) => [(String, String)] -> a -> 'Li :> a
li_A xs = addAttributes xs . Child

link_ :: 'Link > ()
link_ = Child ()

link_A :: [(String, String)] -> 'Link :> ()
link_A xs = addAttributes xs $ Child ()

listing_ :: ('Listing ?> a) => a -> 'Listing > a
listing_ = Child

listing_A :: ('Listing ?> a) => [(String, String)] -> a -> 'Listing :> a
listing_A xs = addAttributes xs . Child

main_ :: ('Main ?> a) => a -> 'Main > a
main_ = Child

main_A :: ('Main ?> a) => [(String, String)] -> a -> 'Main :> a
main_A xs = addAttributes xs . Child

map_ :: ('Map ?> a) => a -> 'Map > a
map_ = Child

map_A :: ('Map ?> a) => [(String, String)] -> a -> 'Map :> a
map_A xs = addAttributes xs . Child

mark_ :: ('Mark ?> a) => a -> 'Mark > a
mark_ = Child

mark_A :: ('Mark ?> a) => [(String, String)] -> a -> 'Mark :> a
mark_A xs = addAttributes xs . Child

marquee_ :: ('Marquee ?> a) => a -> 'Marquee > a
marquee_ = Child

marquee_A :: ('Marquee ?> a) => [(String, String)] -> a -> 'Marquee :> a
marquee_A xs = addAttributes xs . Child

math_ :: ('Math ?> a) => a -> 'Math > a
math_ = Child

math_A :: ('Math ?> a) => [(String, String)] -> a -> 'Math :> a
math_A xs = addAttributes xs . Child

menu_ :: ('Menu ?> a) => a -> 'Menu > a
menu_ = Child

menu_A :: ('Menu ?> a) => [(String, String)] -> a -> 'Menu :> a
menu_A xs = addAttributes xs . Child

menuitem_ :: 'Menuitem > ()
menuitem_ = Child ()

menuitem_A :: [(String, String)] -> 'Menuitem :> ()
menuitem_A xs = addAttributes xs $ Child ()

meta_ :: 'Meta > ()
meta_ = Child ()

meta_A :: [(String, String)] -> 'Meta :> ()
meta_A xs = addAttributes xs $ Child ()

meter_ :: ('Meter ?> a) => a -> 'Meter > a
meter_ = Child

meter_A :: ('Meter ?> a) => [(String, String)] -> a -> 'Meter :> a
meter_A xs = addAttributes xs . Child

multicol_ :: ('Multicol ?> a) => a -> 'Multicol > a
multicol_ = Child

multicol_A :: ('Multicol ?> a) => [(String, String)] -> a -> 'Multicol :> a
multicol_A xs = addAttributes xs . Child

nav_ :: ('Nav ?> a) => a -> 'Nav > a
nav_ = Child

nav_A :: ('Nav ?> a) => [(String, String)] -> a -> 'Nav :> a
nav_A xs = addAttributes xs . Child

nextid_ :: ('Nextid ?> a) => a -> 'Nextid > a
nextid_ = Child

nextid_A :: ('Nextid ?> a) => [(String, String)] -> a -> 'Nextid :> a
nextid_A xs = addAttributes xs . Child

nobr_ :: ('Nobr ?> a) => a -> 'Nobr > a
nobr_ = Child

nobr_A :: ('Nobr ?> a) => [(String, String)] -> a -> 'Nobr :> a
nobr_A xs = addAttributes xs . Child

noembed_ :: ('Noembed ?> a) => a -> 'Noembed > a
noembed_ = Child

noembed_A :: ('Noembed ?> a) => [(String, String)] -> a -> 'Noembed :> a
noembed_A xs = addAttributes xs . Child

noframes_ :: ('Noframes ?> a) => a -> 'Noframes > a
noframes_ = Child

noframes_A :: ('Noframes ?> a) => [(String, String)] -> a -> 'Noframes :> a
noframes_A xs = addAttributes xs . Child

noscript_ :: ('Noscript ?> a) => a -> 'Noscript > a
noscript_ = Child

noscript_A :: ('Noscript ?> a) => [(String, String)] -> a -> 'Noscript :> a
noscript_A xs = addAttributes xs . Child

object_ :: ('Object ?> a) => a -> 'Object > a
object_ = Child

object_A :: ('Object ?> a) => [(String, String)] -> a -> 'Object :> a
object_A xs = addAttributes xs . Child

ol_ :: ('Ol ?> a) => a -> 'Ol > a
ol_ = Child

ol_A :: ('Ol ?> a) => [(String, String)] -> a -> 'Ol :> a
ol_A xs = addAttributes xs . Child

optgroup_ :: ('Optgroup ?> a) => a -> 'Optgroup > a
optgroup_ = Child

optgroup_A :: ('Optgroup ?> a) => [(String, String)] -> a -> 'Optgroup :> a
optgroup_A xs = addAttributes xs . Child

option_ :: ('Option ?> a) => a -> 'Option > a
option_ = Child

option_A :: ('Option ?> a) => [(String, String)] -> a -> 'Option :> a
option_A xs = addAttributes xs . Child

output_ :: ('Output ?> a) => a -> 'Output > a
output_ = Child

output_A :: ('Output ?> a) => [(String, String)] -> a -> 'Output :> a
output_A xs = addAttributes xs . Child

p_ :: ('P ?> a) => a -> 'P > a
p_ = Child

p_A :: ('P ?> a) => [(String, String)] -> a -> 'P :> a
p_A xs = addAttributes xs . Child

param_ :: 'Param > ()
param_ = Child ()

param_A :: [(String, String)] -> 'Param :> ()
param_A xs = addAttributes xs $ Child ()

picture_ :: ('Picture ?> a) => a -> 'Picture > a
picture_ = Child

picture_A :: ('Picture ?> a) => [(String, String)] -> a -> 'Picture :> a
picture_A xs = addAttributes xs . Child

plaintext_ :: ('Plaintext ?> a) => a -> 'Plaintext > a
plaintext_ = Child

plaintext_A :: ('Plaintext ?> a) => [(String, String)] -> a -> 'Plaintext :> a
plaintext_A xs = addAttributes xs . Child

pre_ :: ('Pre ?> a) => a -> 'Pre > a
pre_ = Child

pre_A :: ('Pre ?> a) => [(String, String)] -> a -> 'Pre :> a
pre_A xs = addAttributes xs . Child

progress_ :: ('Progress ?> a) => a -> 'Progress > a
progress_ = Child

progress_A :: ('Progress ?> a) => [(String, String)] -> a -> 'Progress :> a
progress_A xs = addAttributes xs . Child

q_ :: ('Q ?> a) => a -> 'Q > a
q_ = Child

q_A :: ('Q ?> a) => [(String, String)] -> a -> 'Q :> a
q_A xs = addAttributes xs . Child

rp_ :: ('Rp ?> a) => a -> 'Rp > a
rp_ = Child

rp_A :: ('Rp ?> a) => [(String, String)] -> a -> 'Rp :> a
rp_A xs = addAttributes xs . Child

rt_ :: ('Rt ?> a) => a -> 'Rt > a
rt_ = Child

rt_A :: ('Rt ?> a) => [(String, String)] -> a -> 'Rt :> a
rt_A xs = addAttributes xs . Child

rtc_ :: ('Rtc ?> a) => a -> 'Rtc > a
rtc_ = Child

rtc_A :: ('Rtc ?> a) => [(String, String)] -> a -> 'Rtc :> a
rtc_A xs = addAttributes xs . Child

ruby_ :: ('Ruby ?> a) => a -> 'Ruby > a
ruby_ = Child

ruby_A :: ('Ruby ?> a) => [(String, String)] -> a -> 'Ruby :> a
ruby_A xs = addAttributes xs . Child

s_ :: ('S ?> a) => a -> 'S > a
s_ = Child

s_A :: ('S ?> a) => [(String, String)] -> a -> 'S :> a
s_A xs = addAttributes xs . Child

samp_ :: ('Samp ?> a) => a -> 'Samp > a
samp_ = Child

samp_A :: ('Samp ?> a) => [(String, String)] -> a -> 'Samp :> a
samp_A xs = addAttributes xs . Child

script_ :: ('Script ?> a) => a -> 'Script > a
script_ = Child

script_A :: ('Script ?> a) => [(String, String)] -> a -> 'Script :> a
script_A xs = addAttributes xs . Child

section_ :: ('Section ?> a) => a -> 'Section > a
section_ = Child

section_A :: ('Section ?> a) => [(String, String)] -> a -> 'Section :> a
section_A xs = addAttributes xs . Child

select_ :: ('Select ?> a) => a -> 'Select > a
select_ = Child

select_A :: ('Select ?> a) => [(String, String)] -> a -> 'Select :> a
select_A xs = addAttributes xs . Child

shadow_ :: ('Shadow ?> a) => a -> 'Shadow > a
shadow_ = Child

shadow_A :: ('Shadow ?> a) => [(String, String)] -> a -> 'Shadow :> a
shadow_A xs = addAttributes xs . Child

slot_ :: ('Slot ?> a) => a -> 'Slot > a
slot_ = Child

slot_A :: ('Slot ?> a) => [(String, String)] -> a -> 'Slot :> a
slot_A xs = addAttributes xs . Child

small_ :: ('Small ?> a) => a -> 'Small > a
small_ = Child

small_A :: ('Small ?> a) => [(String, String)] -> a -> 'Small :> a
small_A xs = addAttributes xs . Child

source_ :: 'Source > ()
source_ = Child ()

source_A :: [(String, String)] -> 'Source :> ()
source_A xs = addAttributes xs $ Child ()

spacer_ :: ('Spacer ?> a) => a -> 'Spacer > a
spacer_ = Child

spacer_A :: ('Spacer ?> a) => [(String, String)] -> a -> 'Spacer :> a
spacer_A xs = addAttributes xs . Child

span_ :: ('Span ?> a) => a -> 'Span > a
span_ = Child

span_A :: ('Span ?> a) => [(String, String)] -> a -> 'Span :> a
span_A xs = addAttributes xs . Child

strike_ :: ('Strike ?> a) => a -> 'Strike > a
strike_ = Child

strike_A :: ('Strike ?> a) => [(String, String)] -> a -> 'Strike :> a
strike_A xs = addAttributes xs . Child

strong_ :: ('Strong ?> a) => a -> 'Strong > a
strong_ = Child

strong_A :: ('Strong ?> a) => [(String, String)] -> a -> 'Strong :> a
strong_A xs = addAttributes xs . Child

style_ :: ('Style ?> a) => a -> 'Style > a
style_ = Child

style_A :: ('Style ?> a) => [(String, String)] -> a -> 'Style :> a
style_A xs = addAttributes xs . Child

sub_ :: ('Sub ?> a) => a -> 'Sub > a
sub_ = Child

sub_A :: ('Sub ?> a) => [(String, String)] -> a -> 'Sub :> a
sub_A xs = addAttributes xs . Child

summary_ :: ('Summary ?> a) => a -> 'Summary > a
summary_ = Child

summary_A :: ('Summary ?> a) => [(String, String)] -> a -> 'Summary :> a
summary_A xs = addAttributes xs . Child

sup_ :: ('Sup ?> a) => a -> 'Sup > a
sup_ = Child

sup_A :: ('Sup ?> a) => [(String, String)] -> a -> 'Sup :> a
sup_A xs = addAttributes xs . Child

svg_ :: ('Svg ?> a) => a -> 'Svg > a
svg_ = Child

svg_A :: ('Svg ?> a) => [(String, String)] -> a -> 'Svg :> a
svg_A xs = addAttributes xs . Child

table_ :: ('Table ?> a) => a -> 'Table > a
table_ = Child

table_A :: ('Table ?> a) => [(String, String)] -> a -> 'Table :> a
table_A xs = addAttributes xs . Child

tbody_ :: ('Tbody ?> a) => a -> 'Tbody > a
tbody_ = Child

tbody_A :: ('Tbody ?> a) => [(String, String)] -> a -> 'Tbody :> a
tbody_A xs = addAttributes xs . Child

td_ :: ('Td ?> a) => a -> 'Td > a
td_ = Child

td_A :: ('Td ?> a) => [(String, String)] -> a -> 'Td :> a
td_A xs = addAttributes xs . Child

template_ :: ('Template ?> a) => a -> 'Template > a
template_ = Child

template_A :: ('Template ?> a) => [(String, String)] -> a -> 'Template :> a
template_A xs = addAttributes xs . Child

textarea_ :: ('Textarea ?> a) => a -> 'Textarea > a
textarea_ = Child

textarea_A :: ('Textarea ?> a) => [(String, String)] -> a -> 'Textarea :> a
textarea_A xs = addAttributes xs . Child

tfoot_ :: ('Tfoot ?> a) => a -> 'Tfoot > a
tfoot_ = Child

tfoot_A :: ('Tfoot ?> a) => [(String, String)] -> a -> 'Tfoot :> a
tfoot_A xs = addAttributes xs . Child

th_ :: ('Th ?> a) => a -> 'Th > a
th_ = Child

th_A :: ('Th ?> a) => [(String, String)] -> a -> 'Th :> a
th_A xs = addAttributes xs . Child

thead_ :: ('Thead ?> a) => a -> 'Thead > a
thead_ = Child

thead_A :: ('Thead ?> a) => [(String, String)] -> a -> 'Thead :> a
thead_A xs = addAttributes xs . Child

time_ :: ('Time ?> a) => a -> 'Time > a
time_ = Child

time_A :: ('Time ?> a) => [(String, String)] -> a -> 'Time :> a
time_A xs = addAttributes xs . Child

title_ :: ('Title ?> a) => a -> 'Title > a
title_ = Child

title_A :: ('Title ?> a) => [(String, String)] -> a -> 'Title :> a
title_A xs = addAttributes xs . Child

tr_ :: ('Tr ?> a) => a -> 'Tr > a
tr_ = Child

tr_A :: ('Tr ?> a) => [(String, String)] -> a -> 'Tr :> a
tr_A xs = addAttributes xs . Child

track_ :: 'Track > ()
track_ = Child ()

track_A :: [(String, String)] -> 'Track :> ()
track_A xs = addAttributes xs $ Child ()

tt_ :: ('Tt ?> a) => a -> 'Tt > a
tt_ = Child

tt_A :: ('Tt ?> a) => [(String, String)] -> a -> 'Tt :> a
tt_A xs = addAttributes xs . Child

u_ :: ('U ?> a) => a -> 'U > a
u_ = Child

u_A :: ('U ?> a) => [(String, String)] -> a -> 'U :> a
u_A xs = addAttributes xs . Child

ul_ :: ('Ul ?> a) => a -> 'Ul > a
ul_ = Child

ul_A :: ('Ul ?> a) => [(String, String)] -> a -> 'Ul :> a
ul_A xs = addAttributes xs . Child

var_ :: ('Var ?> a) => a -> 'Var > a
var_ = Child

var_A :: ('Var ?> a) => [(String, String)] -> a -> 'Var :> a
var_A xs = addAttributes xs . Child

video_ :: ('Video ?> a) => a -> 'Video > a
video_ = Child

video_A :: ('Video ?> a) => [(String, String)] -> a -> 'Video :> a
video_A xs = addAttributes xs . Child

wbr_ :: 'Wbr > ()
wbr_ = Child ()

wbr_A :: [(String, String)] -> 'Wbr :> ()
wbr_A xs = addAttributes xs $ Child ()

xmp_ :: ('Xmp ?> a) => a -> 'Xmp > a
xmp_ = Child

xmp_A :: ('Xmp ?> a) => [(String, String)] -> a -> 'Xmp :> a
xmp_A xs = addAttributes xs . Child
