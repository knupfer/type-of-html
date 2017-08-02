{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}

module Html.Function where

import Html.Type

import GHC.Exts
import GHC.TypeLits
import Data.Proxy
import Data.Monoid hiding (Last)

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as TLB

-- | Render a html document.  The resulting type can be a String,
-- strict Text, lazy Text, or Builder.  For performance it is
-- recommended to use a lazy Text or a Builder.
--
-- >>> render "a" :: String
-- "a"
--
-- >>> render (div_ "a") :: Text
-- "<div>a</div>"
--
-- For prototyping, there's as well a Show instance:
--
-- >>> i_ "a"
-- <i>a</i>
--
-- Please note the extra quotes for String when using show:
--
-- >>> show "a" == render "a"
-- False
--
-- >>> show img_ == render img_
-- True

{-# INLINE render #-}
render :: forall a b.
  ( Document a
  , Monoid b
  , IsString b
  ) => a -> b
render x = render_ (Tagged x :: Tagged (Symbols a) a ())

  -------------------
  -- internal code --
  -------------------

type Document a =
  ( Renderchunks (Tagged (Symbols a) a ())
  , Renderstring (Tagged (Symbols a) a ())
  , KnownSymbol (Last' (Symbols a))
  )

{-# RULES
"render_/renderB"  render_ = renderB
"render_/renderS"  render_ = renderS
"render_/renderLT" render_ = renderLT
#-}

{-# INLINE [2] render_ #-}
render_ :: forall b prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderstring (Tagged prox val nex)
  , Renderchunks (Tagged prox val nex)
  , Monoid b
  , IsString b
  ) => Tagged prox val nex -> b
render_ x = mconcat $ renderchunks x ++ [closing]
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE renderLT #-}
renderLT :: forall prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged prox val nex)
  ) => Tagged prox val nex -> LT.Text
renderLT x = mconcat $ renderchunks x ++ [closing]
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE renderS #-}
renderS :: forall prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged prox val nex)
  ) => Tagged prox val nex -> String
renderS x = foldr (<>) closing $ renderchunks x
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE renderB #-}
renderB :: forall prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderstring (Tagged prox val nex)
  ) => Tagged prox val nex -> TLB.Builder
renderB x = renderstring x <> closing
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE addAttributes #-}
addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes (Attributes xs) b

class Renderchunks a where
  renderchunks :: (IsString b, Monoid b) => a -> [b]

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a) nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = []

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox val nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = convert (Proxy :: Proxy (HeadL prox))
    : [convert x]

instance Renderchunks (Tagged prox () nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = []

instance
  ( Renderchunks (Tagged (Take (CountContent a) prox) a b)
  , Renderchunks (Tagged (Drop (CountContent a) prox) b nex)
  ) => Renderchunks (Tagged prox (a # b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (a :#: b))
    = renderchunks (Tagged a :: Tagged (Take (CountContent a) prox) a b)
    <> renderchunks (Tagged b :: Tagged (Drop (CountContent a) prox) b nex)

instance {-# OVERLAPPING #-}
  ( Renderchunks (Tagged (Drop 1 prox) (a > b) nex)
  , KnownSymbol (HeadL prox)
  , a ?> b
  ) => Renderchunks (Tagged prox (a :> b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (WithAttributes a b))
    = convert (Proxy :: Proxy (HeadL prox))
    : convert a
    : renderchunks (Tagged (Child b) :: Tagged (Drop 1 prox) (a > b) nex)

instance
  ( Renderchunks (Tagged prox b (Close a))
  ) => Renderchunks (Tagged prox (a > b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (Child b))
    = renderchunks (Tagged b :: Tagged prox b (Close a))

instance
  ( Renderchunks (Tagged (Symbols (Next (a :> b) nex)) (a :> b) ())
  , KnownSymbol (Last' (Symbols (Next (a :> b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox [a :> b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    : concatMap
       (\x -> renderchunks
          (Tagged x :: Tagged (Symbols (Next (a :> b) nex)) (a :> b) ())
          ++ [closing]
       ) xs
    where closing = convert (Proxy :: Proxy (Last' (Symbols (Next (a :> b) nex))))

instance
  ( Renderchunks (Tagged (Symbols (Next (a > b) nex)) (a > b) ())
  , KnownSymbol (Last' (Symbols (Next (a > b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox [a > b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    : concatMap
       (\x -> renderchunks
          (Tagged x :: Tagged (Symbols (Next (a > b) nex)) (a > b) ())
          ++ [closing]
       ) xs
    where closing = convert (Proxy :: Proxy (Last' (Symbols (Next (a > b) nex))))

instance
  ( Renderchunks (Tagged (Symbols (Next (a # b) nex)) (a # b) ())
  , KnownSymbol (Last' (Symbols (Next (a # b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox [a # b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    : concatMap
       (\x -> renderchunks
          (Tagged x :: Tagged (Symbols (Next (a # b) nex)) (a # b) ())
          ++ [closing]
       ) xs
    where closing = convert (Proxy :: Proxy (Last' (Symbols (Next (a # b) nex))))

class Renderstring a where
  renderstring :: (IsString b, Monoid b) => a -> b

instance KnownSymbol a => Renderstring (Tagged prox (Proxy a) nex) where
  {-# INLINE renderstring #-}
  renderstring _ = mempty

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (HeadL prox)
  ) => Renderstring (Tagged prox val nex) where
  {-# INLINE renderstring #-}
  renderstring (Tagged x)
    = convert (undefined :: Proxy (HeadL prox))
    <> convert x

instance Renderstring (Tagged prox () nex) where
  {-# INLINE renderstring #-}
  renderstring _ = mempty

instance
  ( Renderstring (Tagged (Take (CountContent a) prox) a b)
  , Renderstring (Tagged (Drop (CountContent a) prox) b nex)
  ) => Renderstring (Tagged prox (a # b) nex) where
  {-# INLINE renderstring #-}
  renderstring ~(Tagged (a :#: b))
    = renderstring (Tagged a :: Tagged (Take (CountContent a) prox) a b)
    <> renderstring (Tagged b :: Tagged (Drop (CountContent a) prox) b nex)

instance {-# OVERLAPPING #-}
  ( Renderstring (Tagged (Drop 1 prox) (a > b) nex)
  , KnownSymbol (HeadL prox)
  , a ?> b
  ) => Renderstring (Tagged prox (a :> b) nex) where
  {-# INLINE renderstring #-}
  renderstring ~(Tagged (WithAttributes a b))
    = convert (Proxy :: Proxy (HeadL prox))
    <> convert a
    <> renderstring (Tagged (Child b) :: Tagged (Drop 1 prox) (a > b) nex)

instance
  ( Renderstring (Tagged prox b (Close a))
  ) => Renderstring (Tagged prox (a > b) nex) where
  {-# INLINE renderstring #-}
  renderstring ~(Tagged (Child b))
    = renderstring (Tagged b :: Tagged prox b (Close a))

instance
  ( Renderstring (Tagged (Symbols (Next (a :> b) nex)) (a :> b) ())
  , Renderchunks (Tagged (Symbols (Next (a :> b) nex)) (a :> b) ())
  , KnownSymbol (Last' (Symbols (Next (a :> b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderstring (Tagged prox [a :> b] nex) where
  {-# INLINE renderstring #-}
  renderstring (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    <> foldMap
       (\x ->
          render_
          (Tagged x :: Tagged (Symbols (Next (a :> b) nex)) (a :> b) ())
       ) xs

instance
  ( Renderstring (Tagged (Symbols (Next (a > b) nex)) (a > b) ())
  , Renderchunks (Tagged (Symbols (Next (a > b) nex)) (a > b) ())
  , KnownSymbol (Last' (Symbols (Next (a > b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderstring (Tagged prox [a > b] nex) where
  {-# INLINE renderstring #-}
  renderstring (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    <> foldMap
       (\x ->
          render_
          (Tagged x :: Tagged (Symbols (Next (a > b) nex)) (a > b) ())
       ) xs

instance
  ( Renderstring (Tagged (Symbols (Next (a # b) nex)) (a # b) ())
  , Renderchunks (Tagged (Symbols (Next (a # b) nex)) (a # b) ())
  , KnownSymbol (Last' (Symbols (Next (a # b) nex)))
  , KnownSymbol (HeadL prox)
  ) => Renderstring (Tagged prox [a # b] nex) where
  {-# INLINE renderstring #-}
  renderstring (Tagged xs)
    = convert (undefined :: Proxy (HeadL prox))
    <> foldMap
       (\x ->
          render_
          (Tagged x :: Tagged (Symbols (Next (a # b) nex)) (a # b) ())
       ) xs

{-# RULES
"fromString_/builder" fromString_ = TLB.fromLazyText . LT.pack
  #-}

{-# INLINE [2] fromString_ #-}
fromString_ :: IsString a => String -> a
fromString_ = fromString

{-# RULES
"convert/a/a"                   forall f. convert_ f = id
"convert/string/builder"        forall f. convert_ f = TLB.fromLazyText . LT.pack
"convert/lazy text/builder"     forall f. convert_ f = TLB.fromLazyText
"convert/strict text/builder"   forall f. convert_ f = TLB.fromText
"convert/builder/lazy text"     forall f. convert_ f = TLB.toLazyText
"convert/lazy text/strict text" forall f. convert_ f = LT.toStrict
"convert/strict text/lazy text" forall f. convert_ f = LT.fromStrict
  #-}

{-# INLINE [1] convert_ #-}
convert_ :: a -> a
convert_ = id

-- | Convert something to a target stringlike thing.
class Convert a where
  convert :: (IsString b, Monoid b) => a -> b

instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convert #-}
  convert = fromString_ . symbolVal

instance Convert a => Convert (Maybe a) where
  {-# INLINE convert #-}
  convert Nothing = ""
  convert (Just x) = convert x

instance Convert Attributes where
  {-# INLINE convert #-}
  convert ~(Attributes xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

instance Convert String where
  {-# INLINE convert #-}
  convert = convert_ fromString

instance Convert T.Text where
  {-# INLINE convert #-}
  convert = convert_ (fromString . T.unpack)

instance Convert LT.Text where
  {-# INLINE convert #-}
  convert = convert_ (fromString . LT.unpack)

instance Convert TLB.Builder where
  {-# INLINE convert #-}
  convert = convert_ (fromString . LT.unpack . TLB.toLazyText)

instance Convert Int where
  {-# INLINE convert #-}
  convert = fromString . show

instance Convert Integer where
  {-# INLINE convert #-}
  convert = fromString . show

instance Convert Float where
  {-# INLINE convert #-}
  convert = fromString . show

instance Convert Double where
  {-# INLINE convert #-}
  convert = fromString . show

instance Convert Word where
  {-# INLINE convert #-}
  convert = fromString . show

-- | Orphan show instances to faciliate ghci development.
instance                     Document (a # b)  => Show (a # b)  where show = render
instance {-# OVERLAPPING #-} Document [a # b]  => Show [a # b]  where show = render
instance                     Document (a > b)  => Show (a > b)  where show = render
instance {-# OVERLAPPING #-} Document [a > b]  => Show [a > b]  where show = render
instance                     Document (a :> b) => Show (a :> b) where show = render
instance {-# OVERLAPPING #-} Document [a :> b] => Show [a :> b] where show = render
