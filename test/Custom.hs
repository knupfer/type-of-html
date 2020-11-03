{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Custom where

import Html

hxPost_ :: a -> Lawless ("hx-post" := a)
hxPost_ x = Lawless (CustomA := x)
