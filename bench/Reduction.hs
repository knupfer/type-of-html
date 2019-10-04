{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#if   __GLASGOW_HASKELL__ <= 802
{-# OPTIONS_GHC -fsimpl-tick-factor=66 -freduction-depth=55 #-}
#else
{-# OPTIONS_GHC -fsimpl-tick-factor=66 -freduction-depth=29  #-}
#endif

module Main where

import Html
import qualified Html.Attribute as A

main = print p9

p1 = div_A (A.id_ 'a') 'b'

p2 = div_ (p1 # p1)

p3 = div_ (p2 # p2)

p4 = div_ (p3 # p3)

p5 = div_ (p4 # p4)

p6 = div_ (p5 # p5)

p7 = div_ (p6 # p6)

p8 = div_ (p7 # p7)

p9 = div_ (p8 # p8)
