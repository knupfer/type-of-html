{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html

oneElement = (Div :>)

nestedElement x = Div :> Span :> x

parallelElement x = Div :> x # Span :> x

listElement x = [Div :> x]

