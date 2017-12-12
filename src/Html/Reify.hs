{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}

module Html.Reify where

import Html.Type.Internal
import Html.Convert
import Html.CPP

import GHC.TypeLits
import Data.Proxy
import Data.Semigroup ((<>))

class R a where
  render :: a -> Converted

instance
  KnownSymbol s
  => R (Proxy (s :: Symbol)) where
  {-# INLINE render #-}
  render = convert

instance
  R (Proxy ('[] :: [Symbol])) where
  {-# INLINE render #-}
  render _ = mempty

instance
  ( KnownSymbol x, R (Proxy xs)
  ) => R (Proxy ((x ': xs) :: [Symbol])) where
  {-# INLINE render #-}
  render _ = convert (Proxy @ x) <> render (Proxy @ xs)

instance {-# INCOHERENT #-}
  R (T '[] val) where
  {-# INLINE render #-}
  render _ = mempty

instance {-# INCOHERENT #-}
  ( Convert val
  ) => R (T '[ EmptySym ] val) where
  {-# INLINE render #-}
  render (T x) = convert x

instance
  ( Convert b
  , R (Proxy s)
  ) => R (T '[s] (a := b)) where
  {-# INLINE render #-}
  render (T (AT x)) = render (Proxy @ s) <> convert x

instance {-# INCOHERENT #-}
  ( Convert val
  , R (Proxy s)
  ) => R (T '[s] val) where
  {-# INLINE render #-}
  render (T x) = render (Proxy @ s) <> convert x

instance {-# OVERLAPPING #-}
  ( R (T xs val)
  ) => R (T (NoTail xs) val) where
  {-# INLINE render #-}
  render (T t) = render (T t :: T xs val)

instance
  ( R (T xs val)
  , R (Proxy x)
  ) => R (T ('FingerTree xs x) val) where
  {-# INLINE render #-}
  render (T t) = render (T t :: T xs val) <> render (Proxy @ x)

instance
  ( R (T (Take (Length b) ps) b)
  , R (T (Drop (Length b) ps) c)
  ) => R (T ps ((a :@: b) c)) where
  {-# INLINE render #-}
  render (T ~(WithAttributes b c))
    = render (T b :: T (Take (Length b) ps) b)
   <> render (T c :: T (Drop (Length b) ps) c)

instance
  ( R (T (Take (Length a) ps) a)
  , R (T (Drop (Length a) ps) b)
  ) => R (T ps (a # b)) where
  {-# INLINE render #-}
  render (T ~(a :#: b))
    = render (T a :: T (Take (Length a) ps) a)
   <> render (T b :: T (Drop (Length a) ps) b)

instance
  ( R (T (ToList (a `f` b)) (a `f` b))
  , R (Proxy s)
  ) => R (T (s ': ss) [a `f` b]) where
  {-# INLINE render #-}
  render (T xs)
    = render (Proxy @ s)
    <> foldMap (render . newT) xs

instance
  ( R (T (ToList a) a)
  , R (Proxy s)
  ) => R (T (s ': ss) (Maybe a)) where
  {-# INLINE render #-}
  render (T mx)
    = render (Proxy @ s)
    <> foldMap (render . newT) mx

instance
  ( R (T (ToList a) a)
  , R (T (ToList b) b)
  , R (Proxy s)
  ) => R (T (s ': ss) (Either a b)) where
  {-# INLINE render #-}
  render (T eab)
    = render (Proxy @ s)
    <> either (render . newT) (render . newT) eab

newT :: x -> T (ToList x) x
newT = T
