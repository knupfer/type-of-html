{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

#if   __GLASGOW_HASKELL__ == 802
{-# OPTIONS_GHC -freduction-depth=55 #-}
#elif __GLASGOW_HASKELL__ == 804
{-# OPTIONS_GHC -freduction-depth=29 #-}
#elif __GLASGOW_HASKELL__ == 806
{-# OPTIONS_GHC -freduction-depth=29 #-}
#elif __GLASGOW_HASKELL__ == 808
{-# OPTIONS_GHC -freduction-depth=29 #-}
#elif __GLASGOW_HASKELL__ == 810
{-# OPTIONS_GHC -freduction-depth=30 #-}
#else
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif

module Main where

import Html

main = print p9

p1 = Div :@ (IdA := 'a') :> 'b'

p2 = Div :> (p1 # p1)

p3 = Div :> (p2 # p2)

p4 = Div :> (p3 # p3)

p5 = Div :> (p4 # p4)

p6 = Div :> (p5 # p5)

p7 = Div :> (p6 # p6)

p8 = Div :> (p7 # p7)

p9 = Div :> (p8 # p8)
