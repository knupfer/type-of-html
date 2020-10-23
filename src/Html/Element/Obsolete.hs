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
-- >>> :set -XTypeApplications
-- >>> obsolete_ @Center "barf"
-- <center>barf</center>
obsolete_ :: (x ?> a) => a -> x > a
obsolete_ = WithAttributes ()

obsolete_A :: (x <?> a) b => a -> b -> (x :@: a) b
obsolete_A = WithAttributes

type Applet    = Custom "applet"
type Acronym   = Custom "acronym"
type Bgsound   = Custom "bgsound"
type Dir       = Custom "dir"
type Frame     = Custom "frame"
type Frameset  = Custom "frameset"
type Noframes  = Custom "noframes"
type Isindex   = Custom "isindex"
type Keygen    = Custom "keygen"
type Listing   = Custom "listing"
type Menuitem  = Custom "menuitem"
type Nextid    = Custom "nextid"
type Noembed   = Custom "noembed"
type Plaintext = Custom "plaintext"
type Rb        = Custom "rb"
type Rtc       = Custom "rtc"
type Strike    = Custom "strike"
type Xmp       = Custom "xmp"
type Basefont  = Custom "basefont"
type Big       = Custom "big"
type Blink     = Custom "blink"
type Center    = Custom "center"
type Font      = Custom "font"
type Marquee   = Custom "marquee"
type Multicol  = Custom "multicol"
type Nobr      = Custom "nobr"
type Spacer    = Custom "spacer"
type Tt        = Custom "tt"

type Custom x = CustomElement :: Element x '[Metadata, Flow, Sectioning, Heading, Phrasing, Embedded, Interactive, Palpable, Scripting] (Metadata :|: Flow :|: Sectioning :|: Heading :|: Phrasing :|: Embedded :|: Interactive :|: Palpable :|: Scripting) '[]
