{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.String

import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B

{-# INLINE render #-}
render :: forall a b. Document a b => a -> b
render x = mconcat $ renderchunks (Tagged x :: Tagged (Symbols a) a ())
                   <> [unConv (conv (Proxy @ (Last' (Symbols a))))]

-- | Render a html document to a String.
{-# INLINE renderString #-}
renderString :: Document a String => a -> String
renderString = render

-- | Render a html document to a lazy Text.
{-# INLINE renderText #-}
renderText :: Document a T.Text => a -> T.Text
renderText = render

-- | Render a html document to a lazy ByteString.
{-# INLINE renderByteString #-}
renderByteString :: Document a B.ByteString => a -> B.ByteString
renderByteString = render

type Document a b =
  ( Renderchunks (Tagged (Symbols a) a ()) b
  , KnownSymbol (Last' (Symbols a))
  , Conv b
  , Monoid b
  )

class Renderchunks a b where
  renderchunks :: a -> [b]

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a) nex) b where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty
instance Renderchunks (Tagged prox () nex) b where
  {-# INLINE renderchunks #-}
  renderchunks _ = mempty

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , Conv u
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox val nex) u where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = unConv (conv (Proxy @ (HeadL prox)))
    : [unConv (conv x)]

instance
  ( t ~ Tagged prox b (Close a)
  , Renderchunks t u
  ) => Renderchunks (Tagged prox (a > b) nex) u where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(Child b)) = renderchunks (Tagged b :: t)

instance
  ( t ~ Tagged (Drop 1 prox) b (Close a)
  , Renderchunks t u
  , Conv u
  , Monoid u
  , IsString u
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox (a :> b) nex) u where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged (WithAttributes xs b))
    = unConv (conv (Proxy @ (HeadL prox)))
    : foldMap (unConv . conv . Raw . (\(Attribute x) -> x)) xs
    : renderchunks (Tagged b :: t)

instance
  ( t1 ~ Tagged (Take (CountContent a) prox) a b
  , t2 ~ Tagged (Drop (CountContent a) prox) b nex
  , Renderchunks t1 u
  , Renderchunks t2 u
  , Monoid u
  ) => Renderchunks (Tagged prox (a # b) nex) u where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged ~(a :#: b))
    = mconcat (renderchunks (Tagged a :: t1)) : renderchunks (Tagged b :: t2)

instance
  ( t1 ~ Tagged t2 (a `f` b) ()
  , t2 ~ Symbols (Next (a `f` b) nex)
  , Renderchunks t1 u
  , Conv u
  , KnownSymbol (Last' t2)
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox [a `f` b] nex) u where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = unConv (conv (Proxy @ (HeadL prox)))
    : Prelude.concatMap (\x -> renderchunks (Tagged x :: t1) <> [closing]) xs
    where closing = unConv (conv (Proxy @ (Last' t2)))
