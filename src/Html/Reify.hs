{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}

module Html.Reify where

import Html.Type
import Html.Convert

import GHC.TypeLits
import Data.Proxy
import Data.Semigroup

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B

{-# INLINE renderBuilder #-}
renderBuilder :: forall a. Document a => a -> B.Builder
renderBuilder x = renderchunks (Tagged x :: Tagged (Symbols a) a ())
                   <> unConv (convert (Proxy @ (Last' (Symbols a))))

-- | Render a html document to a String.
{-# INLINE renderString #-}
renderString :: Document a => a -> String
renderString = T.unpack . renderText

-- | Render a html document to a lazy Text.
{-# INLINE renderText #-}
renderText :: Document a => a -> T.Text
renderText = T.decodeUtf8 . renderByteString

-- | Render a html document to a lazy ByteString.
{-# INLINE renderByteString #-}
renderByteString :: Document a => a -> B.ByteString
renderByteString = B.toLazyByteString . renderBuilder

type Document a =
  ( Renderchunks (Tagged (Symbols a) a ())
  , KnownSymbol (Last' (Symbols a))
  )

class Renderchunks a where
  renderchunks :: a -> B.Builder

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a) nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty
instance Renderchunks (Tagged prox () nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox val nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = unConv (convert (Proxy @ (HeadL prox)))
    <> unConv (convert x)

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
  renderchunks (Tagged ~(WithAttributes (Attribute x) b))
    = unConv (convert (Proxy @ (HeadL prox)))
    <> x
    <> renderchunks (Tagged b :: t)

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
    = unConv (convert (Proxy @ (HeadL prox)))
    <> foldMap (\x -> renderchunks (Tagged x :: t1) <> closing) xs
    where closing = unConv (convert (Proxy @ (Last' t2)))
          {-# INLINE closing #-}
