{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}

module Html.String where

import Html.Type
import GHC.TypeLits
import Data.Proxy

render :: forall a. Document a => a -> String
render x = convert (Tagged x :: Tagged (Symbols a) a ())
        ++ convert (Proxy @ (Last' (Symbols a)))

escape :: String -> String
escape = concatMap $ \case
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '&'  -> "&amp;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  x    -> [x]

type Document a =
  ( Convert (Tagged (Symbols a) a ())
  , KnownSymbol (Last' (Symbols a))
  )

class Convert a where convert :: a -> String

instance Convert String  where convert = escape
instance Convert Int     where convert = show
instance Convert Integer where convert = show
instance Convert Float   where convert = show
instance Convert Double  where convert = show
instance Convert Word    where convert = show

instance Convert (Tagged prox () nex) where convert _ = ""
instance KnownSymbol a => Convert (Tagged prox (Proxy a) nex) where convert _ = ""
instance KnownSymbol a => Convert (Proxy a) where convert = symbolVal

instance {-# OVERLAPPABLE #-} (Convert val, KnownSymbol (HeadL prox))
  => Convert (Tagged prox val nex) where
  convert (Tagged x) = convert (Proxy @ (HeadL prox)) ++ convert x

instance Convert Attributes where
  convert ~(Attributes xs)
    = concat [ ' ' : a ++ "=\"" ++ escape b ++ "\"" | (a,b) <- xs]

instance
  ( t ~ Tagged prox b (Close a)
  , Convert t
  ) => Convert (Tagged prox (a > b) nex) where
  convert (Tagged ~(Child b)) = convert (Tagged b :: t)

instance
  ( t ~ Tagged (Drop 1 prox) b (Close a)
  , Convert t
  , KnownSymbol (HeadL prox)
  ) => Convert (Tagged prox (a :> b) nex) where
  convert (Tagged ~(WithAttributes a b))
    = convert (Proxy @ (HeadL prox)) ++ convert a ++ convert (Tagged b :: t)

instance
  ( t1 ~ Tagged (Take (CountContent a) prox) a b
  , t2 ~ Tagged (Drop (CountContent a) prox) b nex
  , Convert t1
  , Convert t2
  ) => Convert (Tagged prox (a # b) nex) where
  convert (Tagged ~(a :#: b)) = convert (Tagged a :: t1) ++ convert (Tagged b :: t2)

instance
  ( t1 ~ Tagged t2 (a `f` b) ()
  , t2 ~ Symbols (Next (a `f` b) nex)
  , Convert t1
  , KnownSymbol (Last' t2)
  , KnownSymbol (HeadL prox)
  ) => Convert (Tagged prox [a `f` b] nex) where
  convert (Tagged xs)
    = convert (Proxy @ (HeadL prox))
    ++ concatMap (\x -> convert (Tagged x :: t1)
                     ++ convert (Proxy @ (Last' t2))) xs
