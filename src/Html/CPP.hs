{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE CPP                  #-}

module Html.CPP where

import GHC.TypeLits

#if __GLASGOW_HASKELL__ >= 802
type Sym = Symbol
type List a = '[a]
type EmptySym = ""
type family AppSymbols xs where
  AppSymbols (x ': xs) = AppendSymbol x (AppSymbols xs)
  AppSymbols '[] = ""
#else
type Sym = [Symbol]
type List a = a
type EmptySym = '[]
type AppSymbols xs = xs
#endif
