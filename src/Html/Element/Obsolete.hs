{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}

module Html.Element.Obsolete where

import Html.Type.Internal

-- | Functions to create obsolete html elements.
--
-- Usage:
--
-- >>> center :> "barf"
-- <center>barf</center>

type CustomElementDefault x
  = Element
  x
  '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting] (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
  '[]

applet :: CustomElementDefault "applet"
applet = CustomElement

acronym :: CustomElementDefault "acronym"
acronym = CustomElement

bgsound :: CustomElementDefault "bgsound"
bgsound = CustomElement

dir :: CustomElementDefault "dir"
dir = CustomElement

frame :: CustomElementDefault "frame"
frame = CustomElement

frameset :: CustomElementDefault "frameset"
frameset = CustomElement

noframes :: CustomElementDefault "noframes"
noframes = CustomElement

isindex :: CustomElementDefault "isindex"
isindex = CustomElement

keygen :: CustomElementDefault "keygen"
keygen = CustomElement

listing :: CustomElementDefault "listing"
listing = CustomElement

menuitem :: CustomElementDefault "menuitem"
menuitem = CustomElement

nextid :: CustomElementDefault "nextid"
nextid = CustomElement

noembed :: CustomElementDefault "noembed"
noembed = CustomElement

plaintext :: CustomElementDefault "plaintext"
plaintext = CustomElement

rb :: CustomElementDefault "rb"
rb = CustomElement

rtc :: CustomElementDefault "rtc"
rtc = CustomElement

strike :: CustomElementDefault "strike"
strike = CustomElement

xmp :: CustomElementDefault "xmp"
xmp = CustomElement

basefont :: CustomElementDefault "basefont"
basefont = CustomElement

big :: CustomElementDefault "big"
big = CustomElement

blink :: CustomElementDefault "blink"
blink = CustomElement

center :: CustomElementDefault "center"
center = CustomElement

font :: CustomElementDefault "font"
font = CustomElement

marquee :: CustomElementDefault "marquee"
marquee = CustomElement

multicol :: CustomElementDefault "multicol"
multicol = CustomElement

nobr :: CustomElementDefault "nobr"
nobr = CustomElement

spacer :: CustomElementDefault "spacer"
spacer = CustomElement

tt :: CustomElementDefault "tt"
tt = CustomElement
