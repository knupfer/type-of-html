{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Html.Element where

import Html.Type

doctype_ :: 'DOCTYPE > ()
doctype_ = WithAttributes () ()

a_ :: ('A ?> a) => a -> 'A > a
a_ = WithAttributes ()

a_A :: ('A <?> a) b => a -> b -> ('A :@: a) b
a_A = WithAttributes

abbr_ :: ('Abbr ?> a) => a -> 'Abbr > a
abbr_ = WithAttributes ()

abbr_A :: ('Abbr <?> a) b => a -> b -> ('Abbr :@: a) b
abbr_A = WithAttributes

address_ :: ('Address ?> a) => a -> 'Address > a
address_ = WithAttributes ()

address_A :: ('Address <?> a) b  => a -> b -> ('Address :@: a) b
address_A = WithAttributes

area_ :: 'Area > ()
area_ = WithAttributes () ()

area_A :: ('Area <?> a) () => a -> ('Area :@: a) ()
area_A = flip WithAttributes ()

article_ :: ('Article ?> a) => a -> 'Article > a
article_ = WithAttributes ()

article_A :: ('Article <?> a) b => a -> b -> ('Article :@: a) b
article_A = WithAttributes

aside_ :: ('Aside ?> a) => a -> 'Aside > a
aside_ = WithAttributes ()

aside_A :: ('Aside <?> a) b => a -> b -> ('Aside :@: a) b
aside_A = WithAttributes

audio_ :: ('Audio ?> a) => a -> 'Audio > a
audio_ = WithAttributes ()

audio_A :: ('Audio <?> a) b => a -> b -> ('Audio :@: a) b
audio_A = WithAttributes

b_ :: ('B ?> a) => a -> 'B > a
b_ = WithAttributes ()

b_A :: ('B <?> a) b => a -> b -> ('B :@: a) b
b_A = WithAttributes

base_ :: 'Base > ()
base_ = WithAttributes () ()

base_A :: ('Base <?> a) () => a -> ('Base :@: a) ()
base_A = flip WithAttributes ()

bdi_ :: ('Bdi ?> a) => a -> 'Bdi > a
bdi_ = WithAttributes ()

bdi_A :: ('Bdi <?> a) b => a -> b -> ('Bdi :@: a) b
bdi_A = WithAttributes

bdo_ :: ('Bdo ?> a) => a -> 'Bdo > a
bdo_ = WithAttributes ()

bdo_A :: ('Bdo <?> a) b => a -> b -> ('Bdo :@: a) b
bdo_A = WithAttributes

blockquote_ :: ('Blockquote ?> a) => a -> 'Blockquote > a
blockquote_ = WithAttributes ()

blockquote_A :: ('Blockquote <?> a) b => a -> b -> ('Blockquote :@: a) b
blockquote_A = WithAttributes

body_ :: ('Body ?> a) => a -> 'Body > a
body_ = WithAttributes ()

body_A :: ('Body <?> a) b => a -> b -> ('Body :@: a) b
body_A = WithAttributes

br_ :: 'Br > ()
br_ = WithAttributes () ()

br_A :: ('Br <?> a) () => a -> ('Br :@: a) ()
br_A = flip WithAttributes ()

button_ :: ('Button ?> a) => a -> 'Button > a
button_ = WithAttributes ()

button_A :: ('Button <?> a) b => a -> b -> ('Button :@: a) b
button_A = WithAttributes

canvas_ :: ('Canvas ?> a) => a -> 'Canvas > a
canvas_ = WithAttributes ()

canvas_A :: ('Canvas <?> a) b => a -> b -> ('Canvas :@: a) b
canvas_A = WithAttributes

caption_ :: ('Caption ?> a) => a -> 'Caption > a
caption_ = WithAttributes ()

caption_A :: ('Caption <?> a) b => a -> b -> ('Caption :@: a) b
caption_A = WithAttributes

cite_ :: ('Cite ?> a) => a -> 'Cite > a
cite_ = WithAttributes ()

cite_A :: ('Cite <?> a) b => a -> b -> ('Cite :@: a) b
cite_A = WithAttributes

code_ :: ('Code ?> a) => a -> 'Code > a
code_ = WithAttributes ()

code_A :: ('Code <?> a) b => a -> b -> ('Code :@: a) b
code_A = WithAttributes

col_ :: 'Col > ()
col_ = WithAttributes () ()

col_A :: ('Col <?> a) () => a -> ('Col :@: a) ()
col_A = flip WithAttributes ()

colgroup_ :: ('Colgroup ?> a) => a -> 'Colgroup > a
colgroup_ = WithAttributes ()

colgroup_A :: ('Colgroup <?> a) b => a -> b -> ('Colgroup :@: a) b
colgroup_A = WithAttributes

data_ :: ('Data ?> a) => a -> 'Data > a
data_ = WithAttributes ()

data_A :: ('Data <?> a) b => a -> b -> ('Data :@: a) b
data_A = WithAttributes

datalist_ :: ('Datalist ?> a) => a -> 'Datalist > a
datalist_ = WithAttributes ()

datalist_A :: ('Datalist <?> a) b => a -> b -> ('Datalist :@: a) b
datalist_A = WithAttributes

dd_ :: ('Dd ?> a) => a -> 'Dd > a
dd_ = WithAttributes ()

dd_A :: ('Dd <?> a) b => a -> b -> ('Dd :@: a) b
dd_A = WithAttributes

del_ :: ('Del ?> a) => a -> 'Del > a
del_ = WithAttributes ()

del_A :: ('Del <?> a) b => a -> b -> ('Del :@: a) b
del_A = WithAttributes

details_ :: ('Details ?> a) => a -> 'Details > a
details_ = WithAttributes ()

details_A :: ('Details <?> a) b => a -> b -> ('Details :@: a) b
details_A = WithAttributes

dfn_ :: ('Dfn ?> a) => a -> 'Dfn > a
dfn_ = WithAttributes ()

dfn_A :: ('Dfn <?> a) b => a -> b -> ('Dfn :@: a) b
dfn_A = WithAttributes

dialog_ :: ('Dialog ?> a) => a -> 'Dialog > a
dialog_ = WithAttributes ()

dialog_A :: ('Dialog <?> a) b => a -> b -> ('Dialog :@: a) b
dialog_A = WithAttributes

div_ :: ('Div ?> a) => a -> 'Div > a
div_ = WithAttributes ()

div_A :: ('Div <?> a) b => a -> b -> ('Div :@: a) b
div_A = WithAttributes

dl_ :: ('Dl ?> a) => a -> 'Dl > a
dl_ = WithAttributes ()

dl_A :: ('Dl <?> a) b => a -> b -> ('Dl :@: a) b
dl_A = WithAttributes

dt_ :: ('Dt ?> a) => a -> 'Dt > a
dt_ = WithAttributes ()

dt_A :: ('Dt <?> a) b => a -> b -> ('Dt :@: a) b
dt_A = WithAttributes

em_ :: ('Em ?> a) => a -> 'Em > a
em_ = WithAttributes ()

em_A :: ('Em <?> a) b => a -> b -> ('Em :@: a) b
em_A = WithAttributes

embed_ :: 'Embed > ()
embed_ = WithAttributes () ()

embed_A :: ('Embed <?> a) () => a -> ('Embed :@: a) ()
embed_A = flip WithAttributes ()

fieldset_ :: ('Fieldset ?> a) => a -> 'Fieldset > a
fieldset_ = WithAttributes ()

fieldset_A :: ('Fieldset <?> a) b => a -> b -> ('Fieldset :@: a) b
fieldset_A = WithAttributes

figcaption_ :: ('Figcaption ?> a) => a -> 'Figcaption > a
figcaption_ = WithAttributes ()

figcaption_A :: ('Figcaption <?> a) b => a -> b -> ('Figcaption :@: a) b
figcaption_A = WithAttributes

figure_ :: ('Figure ?> a) => a -> 'Figure > a
figure_ = WithAttributes ()

figure_A :: ('Figure <?> a) b => a -> b -> ('Figure :@: a) b
figure_A = WithAttributes

footer_ :: ('Footer ?> a) => a -> 'Footer > a
footer_ = WithAttributes ()

footer_A :: ('Footer <?> a) b => a -> b -> ('Footer :@: a) b
footer_A = WithAttributes

form_ :: ('Form ?> a) => a -> 'Form > a
form_ = WithAttributes ()

form_A :: ('Form <?> a) b => a -> b -> ('Form :@: a) b
form_A = WithAttributes

h1_ :: ('H1 ?> a) => a -> 'H1 > a
h1_ = WithAttributes ()

h1_A :: ('H1 <?> a) b => a -> b -> ('H1 :@: a) b
h1_A = WithAttributes

h2_ :: ('H2 ?> a) => a -> 'H2 > a
h2_ = WithAttributes ()

h2_A :: ('H2 <?> a) b => a -> b -> ('H2 :@: a) b
h2_A = WithAttributes

h3_ :: ('H3 ?> a) => a -> 'H3 > a
h3_ = WithAttributes ()

h3_A :: ('H3 <?> a) b => a -> b -> ('H3 :@: a) b
h3_A = WithAttributes

h4_ :: ('H4 ?> a) => a -> 'H4 > a
h4_ = WithAttributes ()

h4_A :: ('H4 <?> a) b => a -> b -> ('H4 :@: a) b
h4_A = WithAttributes

h5_ :: ('H5 ?> a) => a -> 'H5 > a
h5_ = WithAttributes ()

h5_A :: ('H5 <?> a) b => a -> b -> ('H5 :@: a) b
h5_A = WithAttributes

h6_ :: ('H6 ?> a) => a -> 'H6 > a
h6_ = WithAttributes ()

h6_A :: ('H6 <?> a) b => a -> b -> ('H6 :@: a) b
h6_A = WithAttributes

head_ :: ('Head ?> a) => a -> 'Head > a
head_ = WithAttributes ()

head_A :: ('Head <?> a) b => a -> b -> ('Head :@: a) b
head_A = WithAttributes

header_ :: ('Header ?> a) => a -> 'Header > a
header_ = WithAttributes ()

header_A :: ('Header <?> a) b => a -> b -> ('Header :@: a) b
header_A = WithAttributes

hgroup_ :: ('Hgroup ?> a) => a -> 'Hgroup > a
hgroup_ = WithAttributes ()

hgroup_A :: ('Hgroup <?> a) b => a -> b -> ('Hgroup :@: a) b
hgroup_A = WithAttributes

hr_ :: 'Hr > ()
hr_ = WithAttributes () ()

hr_A :: ('Hr <?> a) () => a -> ('Hr :@: a) ()
hr_A = flip WithAttributes ()

html_ :: ('Html ?> a) => a -> 'Html > a
html_ = WithAttributes ()

html_A :: ('Html <?> a) b => a -> b -> ('Html :@: a) b
html_A = WithAttributes

i_ :: ('I ?> a) => a -> 'I > a
i_ = WithAttributes ()

i_A :: ('I <?> a) b => a -> b -> ('I :@: a) b
i_A = WithAttributes

iframe_ :: 'Iframe > ()
iframe_ = WithAttributes () ()

iframe_A :: ('Iframe <?> a) () => a -> ('Iframe :@: a) ()
iframe_A = flip WithAttributes ()

img_ :: 'Img > ()
img_ = WithAttributes () ()

img_A :: ('Img <?> a) () => a -> ('Img :@: a) ()
img_A = flip WithAttributes ()

input_ :: 'Input > ()
input_ = WithAttributes () ()

input_A :: ('Input <?> a) () => a -> ('Input :@: a) ()
input_A = flip WithAttributes ()

ins_ :: ('Ins ?> a) => a -> 'Ins > a
ins_ = WithAttributes ()

ins_A :: ('Ins <?> a) b => a -> b -> ('Ins :@: a) b
ins_A = WithAttributes

kbd_ :: ('Kbd ?> a) => a -> 'Kbd > a
kbd_ = WithAttributes ()

kbd_A :: ('Kbd <?> a) b => a -> b -> ('Kbd :@: a) b
kbd_A = WithAttributes

label_ :: ('Label ?> a) => a -> 'Label > a
label_ = WithAttributes ()

label_A :: ('Label <?> a) b => a -> b -> ('Label :@: a) b
label_A = WithAttributes

legend_ :: ('Legend ?> a) => a -> 'Legend > a
legend_ = WithAttributes ()

legend_A :: ('Legend <?> a) b => a -> b -> ('Legend :@: a) b
legend_A = WithAttributes

li_ :: ('Li ?> a) => a -> 'Li > a
li_ = WithAttributes ()

li_A :: ('Li <?> a) b => a -> b -> ('Li :@: a) b
li_A = WithAttributes

link_ :: 'Link > ()
link_ = WithAttributes () ()

link_A :: ('Link <?> a) () => a -> ('Link :@: a) ()
link_A = flip WithAttributes ()

main_ :: ('Main ?> a) => a -> 'Main > a
main_ = WithAttributes ()

main_A :: ('Main <?> a) b => a -> b -> ('Main :@: a) b
main_A = WithAttributes

map_ :: ('Map ?> a) => a -> 'Map > a
map_ = WithAttributes ()

map_A :: ('Map <?> a) b => a -> b -> ('Map :@: a) b
map_A = WithAttributes

mark_ :: ('Mark ?> a) => a -> 'Mark > a
mark_ = WithAttributes ()

mark_A :: ('Mark <?> a) b => a -> b -> ('Mark :@: a) b
mark_A = WithAttributes

menu_ :: ('Menu ?> a) => a -> 'Menu > a
menu_ = WithAttributes ()

menu_A :: ('Menu <?> a) b => a -> b -> ('Menu :@: a) b
menu_A = WithAttributes

meta_ :: 'Meta > ()
meta_ = WithAttributes () ()

meta_A :: ('Meta <?> a) () => a -> ('Meta :@: a) ()
meta_A = flip WithAttributes ()

meter_ :: ('Meter ?> a) => a -> 'Meter > a
meter_ = WithAttributes ()

meter_A :: ('Meter <?> a) b => a -> b -> ('Meter :@: a) b
meter_A = WithAttributes

nav_ :: ('Nav ?> a) => a -> 'Nav > a
nav_ = WithAttributes ()

nav_A :: ('Nav <?> a) b => a -> b -> ('Nav :@: a) b
nav_A = WithAttributes

noscript_ :: ('Noscript ?> a) => a -> 'Noscript > a
noscript_ = WithAttributes ()

noscript_A :: ('Noscript <?> a) b => a -> b -> ('Noscript :@: a) b
noscript_A = WithAttributes

object_ :: ('Object ?> a) => a -> 'Object > a
object_ = WithAttributes ()

object_A :: ('Object <?> a) b => a -> b -> ('Object :@: a) b
object_A = WithAttributes

ol_ :: ('Ol ?> a) => a -> 'Ol > a
ol_ = WithAttributes ()

ol_A :: ('Ol <?> a) b => a -> b -> ('Ol :@: a) b
ol_A = WithAttributes

optgroup_ :: ('Optgroup ?> a) => a -> 'Optgroup > a
optgroup_ = WithAttributes ()

optgroup_A :: ('Optgroup <?> a) b => a -> b -> ('Optgroup :@: a) b
optgroup_A = WithAttributes

option_ :: ('Option ?> a) => a -> 'Option > a
option_ = WithAttributes ()

option_A :: ('Option <?> a) b => a -> b -> ('Option :@: a) b
option_A = WithAttributes

output_ :: ('Output ?> a) => a -> 'Output > a
output_ = WithAttributes ()

output_A :: ('Output <?> a) b => a -> b -> ('Output :@: a) b
output_A = WithAttributes

p_ :: ('P ?> a) => a -> 'P > a
p_ = WithAttributes ()

p_A :: ('P <?> a) b => a -> b -> ('P :@: a) b
p_A = WithAttributes

param_ :: 'Param > ()
param_ = WithAttributes () ()

param_A :: ('Param <?> a) () => a -> ('Param :@: a) ()
param_A = flip WithAttributes ()

picture_ :: ('Picture ?> a) => a -> 'Picture > a
picture_ = WithAttributes ()

picture_A :: ('Picture <?> a) b => a -> b -> ('Picture :@: a) b
picture_A = WithAttributes

pre_ :: ('Pre ?> a) => a -> 'Pre > a
pre_ = WithAttributes ()

pre_A :: ('Pre <?> a) b => a -> b -> ('Pre :@: a) b
pre_A = WithAttributes

progress_ :: ('Progress ?> a) => a -> 'Progress > a
progress_ = WithAttributes ()

progress_A :: ('Progress <?> a) b => a -> b -> ('Progress :@: a) b
progress_A = WithAttributes

q_ :: ('Q ?> a) => a -> 'Q > a
q_ = WithAttributes ()

q_A :: ('Q <?> a) b => a -> b -> ('Q :@: a) b
q_A = WithAttributes

rp_ :: ('Rp ?> a) => a -> 'Rp > a
rp_ = WithAttributes ()

rp_A :: ('Rp <?> a) b => a -> b -> ('Rp :@: a) b
rp_A = WithAttributes

rt_ :: ('Rt ?> a) => a -> 'Rt > a
rt_ = WithAttributes ()

rt_A :: ('Rt <?> a) b => a -> b -> ('Rt :@: a) b
rt_A = WithAttributes

ruby_ :: ('Ruby ?> a) => a -> 'Ruby > a
ruby_ = WithAttributes ()

ruby_A :: ('Ruby <?> a) b => a -> b -> ('Ruby :@: a) b
ruby_A = WithAttributes

s_ :: ('S ?> a) => a -> 'S > a
s_ = WithAttributes ()

s_A :: ('S <?> a) b => a -> b -> ('S :@: a) b
s_A = WithAttributes

samp_ :: ('Samp ?> a) => a -> 'Samp > a
samp_ = WithAttributes ()

samp_A :: ('Samp <?> a) b => a -> b -> ('Samp :@: a) b
samp_A = WithAttributes

script_ :: ('Script ?> a) => a -> 'Script > a
script_ = WithAttributes ()

script_A :: ('Script <?> a) b => a -> b -> ('Script :@: a) b
script_A = WithAttributes

section_ :: ('Section ?> a) => a -> 'Section > a
section_ = WithAttributes ()

section_A :: ('Section <?> a) b => a -> b -> ('Section :@: a) b
section_A = WithAttributes

select_ :: ('Select ?> a) => a -> 'Select > a
select_ = WithAttributes ()

select_A :: ('Select <?> a) b => a -> b -> ('Select :@: a) b
select_A = WithAttributes

slot_ :: ('Slot ?> a) => a -> 'Slot > a
slot_ = WithAttributes ()

slot_A :: ('Slot <?> a) b => a -> b -> ('Slot :@: a) b
slot_A = WithAttributes

small_ :: ('Small ?> a) => a -> 'Small > a
small_ = WithAttributes ()

small_A :: ('Small <?> a) b => a -> b -> ('Small :@: a) b
small_A = WithAttributes

source_ :: 'Source > ()
source_ = WithAttributes () ()

source_A :: ('Source <?> a) () => a -> ('Source :@: a) ()
source_A = flip WithAttributes ()

span_ :: ('Span ?> a) => a -> 'Span > a
span_ = WithAttributes ()

span_A :: ('Span <?> a) b => a -> b -> ('Span :@: a) b
span_A = WithAttributes

strong_ :: ('Strong ?> a) => a -> 'Strong > a
strong_ = WithAttributes ()

strong_A :: ('Strong <?> a) b => a -> b -> ('Strong :@: a) b
strong_A = WithAttributes

style_ :: ('Style ?> a) => a -> 'Style > a
style_ = WithAttributes ()

style_A :: ('Style <?> a) b => a -> b -> ('Style :@: a) b
style_A = WithAttributes

sub_ :: ('Sub ?> a) => a -> 'Sub > a
sub_ = WithAttributes ()

sub_A :: ('Sub <?> a) b => a -> b -> ('Sub :@: a) b
sub_A = WithAttributes

summary_ :: ('Summary ?> a) => a -> 'Summary > a
summary_ = WithAttributes ()

summary_A :: ('Summary <?> a) b => a -> b -> ('Summary :@: a) b
summary_A = WithAttributes

sup_ :: ('Sup ?> a) => a -> 'Sup > a
sup_ = WithAttributes ()

sup_A :: ('Sup <?> a) b => a -> b -> ('Sup :@: a) b
sup_A = WithAttributes

table_ :: ('Table ?> a) => a -> 'Table > a
table_ = WithAttributes ()

table_A :: ('Table <?> a) b => a -> b -> ('Table :@: a) b
table_A = WithAttributes

tbody_ :: ('Tbody ?> a) => a -> 'Tbody > a
tbody_ = WithAttributes ()

tbody_A :: ('Tbody <?> a) b => a -> b -> ('Tbody :@: a) b
tbody_A = WithAttributes

td_ :: ('Td ?> a) => a -> 'Td > a
td_ = WithAttributes ()

td_A :: ('Td <?> a) b => a -> b -> ('Td :@: a) b
td_A = WithAttributes

template_ :: ('Template ?> a) => a -> 'Template > a
template_ = WithAttributes ()

template_A :: ('Template <?> a) b => a -> b -> ('Template :@: a) b
template_A = WithAttributes

textarea_ :: ('Textarea ?> a) => a -> 'Textarea > a
textarea_ = WithAttributes ()

textarea_A :: ('Textarea <?> a) b => a -> b -> ('Textarea :@: a) b
textarea_A = WithAttributes

tfoot_ :: ('Tfoot ?> a) => a -> 'Tfoot > a
tfoot_ = WithAttributes ()

tfoot_A :: ('Tfoot <?> a) b => a -> b -> ('Tfoot :@: a) b
tfoot_A = WithAttributes

th_ :: ('Th ?> a) => a -> 'Th > a
th_ = WithAttributes ()

th_A :: ('Th <?> a) b => a -> b -> ('Th :@: a) b
th_A = WithAttributes

thead_ :: ('Thead ?> a) => a -> 'Thead > a
thead_ = WithAttributes ()

thead_A :: ('Thead <?> a) b => a -> b -> ('Thead :@: a) b
thead_A = WithAttributes

time_ :: ('Time ?> a) => a -> 'Time > a
time_ = WithAttributes ()

time_A :: ('Time <?> a) b => a -> b -> ('Time :@: a) b
time_A = WithAttributes

title_ :: ('Title ?> a) => a -> 'Title > a
title_ = WithAttributes ()

title_A :: ('Title <?> a) b => a -> b -> ('Title :@: a) b
title_A = WithAttributes

tr_ :: ('Tr ?> a) => a -> 'Tr > a
tr_ = WithAttributes ()

tr_A :: ('Tr <?> a) b => a -> b -> ('Tr :@: a) b
tr_A = WithAttributes

track_ :: 'Track > ()
track_ = WithAttributes () ()

track_A :: ('Track <?> a) () => a -> ('Track :@: a) ()
track_A = flip WithAttributes ()

u_ :: ('U ?> a) => a -> 'U > a
u_ = WithAttributes ()

u_A :: ('U <?> a) b => a -> b -> ('U :@: a) b
u_A = WithAttributes

ul_ :: ('Ul ?> a) => a -> 'Ul > a
ul_ = WithAttributes ()

ul_A :: ('Ul <?> a) b => a -> b -> ('Ul :@: a) b
ul_A = WithAttributes

var_ :: ('Var ?> a) => a -> 'Var > a
var_ = WithAttributes ()

var_A :: ('Var <?> a) b => a -> b -> ('Var :@: a) b
var_A = WithAttributes

video_ :: ('Video ?> a) => a -> 'Video > a
video_ = WithAttributes ()

video_A :: ('Video <?> a) b => a -> b -> ('Video :@: a) b
video_A = WithAttributes

wbr_ :: 'Wbr > ()
wbr_ = WithAttributes () ()

wbr_A :: ('Wbr <?> a) () => a -> ('Wbr :@: a) ()
wbr_A = flip WithAttributes ()
