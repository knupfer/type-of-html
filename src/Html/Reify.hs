{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE GADTs                  #-}

module Html.Reify where

import Html.Type.Internal
import Html.Convert

import Data.Proxy
import Data.Semigroup ((<>))
import GHC.TypeLits
import Data.ByteString.Builder

import qualified Data.Sequence as S

type Compactable' a = (ShowTypeList (Reverse (Variables a)), R 'True (T (ToList a) a))

-- | Constraint for compactable html documents.  It's a type family to avoid an
-- error about FlexibleContexts and a warning about MonoLocalBinds.
type family Compactable a where Compactable a = Compactable' a

-- | Data for putting variables into a rendered compacted html document.
data Put (n :: Symbol) = forall a. Convert a => Put a

-- | Type of a rendered compact html which determines the amount of arguments.
type family Retrieve f xs where
  Retrieve f (x ': xs) = Put x -> Retrieve f xs
  Retrieve f '[] = f

-- | List of Symbols for which a render function can be created.
class Retrievable a where
  retrieve :: [Builder] -> (Builder -> f) -> CompactHTML a -> Retrieve f a

instance (KnownSymbol x, Retrievable xs) => Retrievable (x ': xs) where
  retrieve m f (MkCompactHTML c1 c2) (Put x) = retrieve (unConv (convert x) : m) f (MkCompactHTML @ xs c1 c2)

instance Retrievable '[] where
  {-# INLINE retrieve #-}
  retrieve m f (MkCompactHTML bs is) = f $ byteString bs <> foldMap (\(i,b) -> m !! i <> byteString b) is

type Document' a = R 'False (T (ToList a) a)

-- | Constraint for html documents.  It's a type family to avoid an
-- error about FlexibleContexts and a warning about MonoLocalBinds.
type family Document a where Document a = Document' a

type family RenderOutput x = r | r -> x where
  RenderOutput 'False = Converted
  RenderOutput 'True = S.Seq (Either Converted String)

class R u a where
  render :: a -> RenderOutput u

instance Convert s
  => R 'False (One s) where
  {-# INLINE render #-}
  render (One x) = convert x

instance Convert s
  => R 'True (One s) where
  {-# INLINE render #-}
  render (One x) = pure . Left $ convert x

instance {-# INCOHERENT #-}
  KnownSymbol n =>
  R 'True (T '[ "" ] (V n)) where
  {-# INLINE render #-}
  render _ = pure (Right (symbolVal (Proxy @ n)))

-- | Common instances

instance {-# INCOHERENT #-}
  Monoid (RenderOutput u) => R u (T '[] val) where
  {-# INLINE render #-}
  render _ = mempty

instance {-# INCOHERENT #-}
  ( R u (One val)
  ) => R u (T '[ "" ] val) where
  {-# INLINE render #-}
  render (T x) = render (One x)

instance
  ( R u (T '[ "" ] b)
  , R u (One (Proxy s))
  , Semigroup (RenderOutput u)
  ) => R u (T '[s] (a := b)) where
  {-# INLINE render #-}
  render (T (AT x)) = render (One (Proxy @ s)) <> render (T x :: T '[ "" ] b)

instance {-# INCOHERENT #-}
  ( R u (T '[ "" ] val)
  , R u (One (Proxy s))
  , Semigroup (RenderOutput u)
  ) => R u (T '[s] val) where
  {-# INLINE render #-}
  render (T x) = render (One (Proxy @ s)) <> render (T x :: T '[ "" ] val)

instance {-# OVERLAPPING #-}
  ( R u (One (Proxy s))
  , R u (One String)
  , Semigroup (RenderOutput u)
  ) => R u (T '[s] String) where
  {-# INLINE render #-}
  render (T x) = render (One (Proxy @ s)) <> render (One x)

instance {-# OVERLAPPING #-}
  ( R u (T xs val)
  ) => R u (T ('List xs "") val) where
  {-# INLINE render #-}
  render (T t) = render (T t :: T xs val)

instance
  ( R u (T xs val)
  , R u (One (Proxy x))
  , Semigroup (RenderOutput u)
  ) => R u (T ('List xs x) val) where
  {-# INLINE render #-}
  render (T t) = render (T t :: T xs val) <> render (One (Proxy @ x))

instance
  ( R u (T (Take (Length b) ps) b)
  , R u (T (Drop (Length b) ps) c)
  , Semigroup (RenderOutput u)
  ) => R u (T ps ((a :@: b) c)) where
  {-# INLINE render #-}
  render (T ~(WithAttributes b c))
    = render (T b :: T (Take (Length b) ps) b)
    <> render (T c :: T (Drop (Length b) ps) c)

instance
  ( R u (T (Take (Length a) ps) a)
  , R u (T (Drop (Length a) ps) b)
  , Semigroup (RenderOutput u)
  ) => R u (T ps (a # b)) where
  {-# INLINE render #-}
  render (T ~(a :#: b))
    = render (T a :: T (Take (Length a) ps) a)
    <> render (T b :: T (Drop (Length a) ps) b)

instance
  ( R u (T (ToList a) a)
  , R u (One (Proxy s))
  , Monoid (RenderOutput u)
  ) => R u (T (s ': ss) [a]) where
  {-# INLINE render #-}
  render (T xs)
    = render (One (Proxy @ s))
    <> foldMap (render . (T :: a -> T (ToList a) a)) xs

instance
  ( R u (T (ToList a) a)
  , R u (One (Proxy s))
  , Monoid (RenderOutput u)
  ) => R u (T (s ': ss) (Maybe a)) where
  {-# INLINE render #-}
  render (T mx)
    = render (One (Proxy @ s))
    <> foldMap (render . (T :: a -> T (ToList a) a)) mx

instance
  ( R u (T (ToList a) a)
  , R u (T (ToList b) b)
  , R u (One (Proxy s))
  , Semigroup (RenderOutput u)
  ) => R u (T (s ': ss) (Either a b)) where
  {-# INLINE render #-}
  render (T eab)
    = render (One (Proxy @ s))
    <> either (render . (T :: a -> T (ToList a) a)) (render . (T :: b -> T (ToList b) b)) eab
