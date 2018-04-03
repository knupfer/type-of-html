{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}

module Compilation (helloWorld, R(..), Proxy(..)) where

import Html

import GHC.TypeLits
import Data.Proxy

type family Repeat (n :: Nat) x where
  Repeat 0 x = x
  Repeat n x = x # Repeat (n-1) x

class R n x where
  r :: Proxy n -> x -> Repeat n x

instance {-# INCOHERENT #-} R 0 x where
  r _ x = x

instance (Repeat n x ~ (x # Repeat (n-1) x), R (n-1) x) => R n x where
  r _ x = x # r (Proxy :: Proxy (n-1)) x

helloWorld :: 'Title ?> a => a -> 'Html > (('Head > ('Title > a)) # ('Body > ('P > ())))
helloWorld x =
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ ()
      )
    )


