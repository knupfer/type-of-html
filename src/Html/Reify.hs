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
import Data.Semigroup ((<>))

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy    as B
import qualified Data.ByteString.Builder as B

-- | Render a html document to a Builder.
{-# INLINE renderBuilder #-}
renderBuilder :: forall a. Document a => a -> B.Builder
renderBuilder x = renderchunks (Tagged x :: Tagged (Symbols a) a)
                   <> unConv (convert (Proxy @ (Last (Symbols a))))

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
  ( Renderchunks (Tagged (Symbols a) a)
  , KnownSymbol (Last (Symbols a))
  )

class Renderchunks a where
  renderchunks :: a -> B.Builder

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a)) where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty
instance Renderchunks (Tagged prox ()) where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty

instance {-# INCOHERENT #-}
  ( Convert val
  ) => Renderchunks (Tagged ("" ': ss) val) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = unConv (convert x)

instance {-# INCOHERENT #-}
  ( Convert val
  , KnownSymbol s
  ) => Renderchunks (Tagged (s ': ss) val) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = unConv (convert (Proxy @ s))
    <> unConv (convert x)

instance
  ( Renderchunks (Tagged prox b)
  ) => Renderchunks (Tagged prox (a > b)) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(Child b)) = renderchunks (Tagged b :: Tagged prox b)

instance
  ( Renderchunks (Tagged (Take (CountContent b) prox) b)
  , Renderchunks (Tagged (Drop (CountContent b) prox) c)
  ) => Renderchunks (Tagged prox ((a :@: b) c)) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(WithAttributes b c))
    = renderchunks (Tagged b :: Tagged (Take (CountContent b) prox) b)
   <> renderchunks (Tagged c :: Tagged (Drop (CountContent b) prox) c)

instance
  ( Renderchunks (Tagged (Take (CountContent a) prox) a)
  , Renderchunks (Tagged (Drop (CountContent a) prox) b)
  ) => Renderchunks (Tagged prox (a # b)) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(a :#: b))
    = renderchunks (Tagged a :: Tagged (Take (CountContent a) prox) a)
   <> renderchunks (Tagged b :: Tagged (Drop (CountContent a) prox) b)

instance
  ( Renderchunks (Tagged (Symbols (a `f` b)) (a `f` b))
  , KnownSymbol (Last (Symbols (a `f` b)))
  , KnownSymbol s
  ) => Renderchunks (Tagged (s ': ss) [a `f` b]) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = unConv (convert (Proxy @ s))
    <> foldMap (\x -> renderchunks (Tagged x :: Tagged (Symbols (a `f` b)) (a `f` b)) <> closing) xs
    where closing = unConv (convert (Proxy @ (Last (Symbols (a `f` b)))))

