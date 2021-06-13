{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}

module Html.Obsolete
  ( module Html.Obsolete
  , module Html.Type
  ) where

import Html.Type

-- | Obsolete html elements.
--
-- Usage:
--
-- >>> Center :> "barf"
-- <center>barf</center>

data instance Element
    "applet"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Applet

data instance Element
    "acronym"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Acronym

data instance Element
    "bgsound"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Bgsound

data instance Element
    "dir"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Dir

data instance Element
    "frame"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Frame

data instance Element
    "frameset"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Frameset

data instance Element
    "noframes"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Noframes

data instance Element
    "isindex"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Isindex

data instance Element
    "keygen"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Keygen

data instance Element
    "listing"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Listing

data instance Element
    "menuitem"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Menuitem

data instance Element
    "nextid"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Nextid

data instance Element
    "noembed"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Noembed

data instance Element
    "plaintext"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Plaintext

data instance Element
    "rb"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Rb

data instance Element
    "rtc"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Rtc

data instance Element
    "strike"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Strike

data instance Element
    "xmp"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Xmp

data instance Element
    "basefont"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Basefont

data instance Element
    "big"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Big

data instance Element
    "blink"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Blink

data instance Element
    "center"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Center

data instance Element
    "font"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Font

data instance Element
    "marquee"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Marquee

data instance Element
    "multicol"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Multicol

data instance Element
    "nobr"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Nobr

data instance Element
    "spacer"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Spacer

data instance Element
    "tt"
    '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting]
    (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting)
    '[]
  = Tt

