{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}

module Html.Text where

import Html.Type
import Html.Convert

import GHC.TypeLits
import Data.Proxy
import Data.Semigroup

import Data.Text.Lazy as T

{-# INLINE render #-}
render :: forall a. Document a => a -> Text
render x = T.concat $ renderchunks (Tagged x :: Tagged (Symbols a) a ())
                   <> [convertText (Proxy @ (Last' (Symbols a)))]

type Document a =
  ( Renderchunks (Tagged (Symbols a) a ())
  , KnownSymbol (Last' (Symbols a))
  )

class Renderchunks a where
  renderchunks :: a -> [Text]

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a) nex) where renderchunks _ = mempty
instance Renderchunks (Tagged prox () nex) where renderchunks _ = mempty

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox val nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = convertText (Proxy @ (HeadL prox))
    : [convertText x]

instance
  ( t ~ Tagged prox b (Close a)
  , Renderchunks t
  ) => Renderchunks (Tagged prox (a > b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(Child b)) = renderchunks (Tagged b :: t)

instance
  ( t ~ Tagged (Drop 1 prox) b (Close a)
  , Renderchunks t
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox (a :> b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(WithAttributes a b))
    = convertText (Proxy @ (HeadL prox))
    : convertText a
    : renderchunks (Tagged b :: t)

instance
  ( t1 ~ Tagged (Take (CountContent a) prox) a b
  , t2 ~ Tagged (Drop (CountContent a) prox) b nex
  , Renderchunks t1
  , Renderchunks t2
  ) => Renderchunks (Tagged prox (a # b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(a :#: b))
    = renderchunks (Tagged a :: t1) <> renderchunks (Tagged b :: t2)

instance
  ( t1 ~ Tagged t2 (a `f` b) ()
  , t2 ~ Symbols (Next (a `f` b) nex)
  , Renderchunks t1
  , KnownSymbol (Last' t2)
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox [a `f` b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convertText (Proxy @ (HeadL prox))
    : Prelude.concatMap (\x -> renderchunks (Tagged x :: t1) <> [closing]) xs
    where closing = convertText (Proxy @ (Last' t2))
