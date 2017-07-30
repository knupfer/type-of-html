{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}

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
render x = render' (Tagged x :: Tagged 0 (SymbolsToList (Symbols a)) a ())

  -------------------
  -- internal code --
  -------------------

type Document a =
  ( Renderchunks (Tagged 0 (SymbolsToList (Symbols a)) a ())
  , KnownSymbol (Last' (SymbolsToList (Symbols a)))
  )

{-# RULES
"render'/renderB"     render' = renderB
"render'/renderS"     render' = renderS
"render'/render_" [2] render' = render_
  #-}

{-# NOINLINE render' #-}
render' :: forall b pos prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged pos prox val nex)
  , Monoid b
  , IsString b
  ) => Tagged pos prox val nex -> b
render' x = mconcat $ renderchunks x ++ [closing]
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE render_ #-}
render_ :: forall b pos prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged pos prox val nex)
  , Monoid b
  , IsString b
  ) => Tagged pos prox val nex -> b
render_ x = mconcat $ renderchunks x ++ [closing]
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE renderB #-}
renderB :: forall pos prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged pos prox val nex)
  ) => Tagged pos prox val nex -> TLB.Builder
renderB x = foldr (<>) closing (renderchunks x)
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE renderS #-}
renderS :: forall pos prox val nex.
  ( KnownSymbol (Last' prox)
  , Renderchunks (Tagged pos prox val nex)
  ) => Tagged pos prox val nex -> String
renderS x = foldr (<>) closing (renderchunks x)
  where closing = convert (Proxy :: Proxy (Last' prox))

{-# INLINE addAttributes #-}
addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes (Attributes xs) b

class Renderchunks a where
  renderchunks :: (IsString b, Monoid b) => a -> [b]

instance KnownSymbol a => Renderchunks (Tagged pos prox (Proxy a) nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = []

instance
  ( KnownSymbol a
  ) => Renderchunks (Tagged pos prox [Proxy a] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs) = foldMap (renderchunks . Tagged) xs

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (Index pos prox)
  ) => Renderchunks (Tagged pos prox val nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = convert (undefined :: Proxy (Index pos prox))
    : [convert x]

instance Renderchunks (Tagged pos prox () nex) where
  {-# INLINE renderchunks #-}
  renderchunks _ = []

instance
  ( Renderchunks (Tagged pos prox a b)
  , Renderchunks (Tagged (CountContent a + pos) prox b nex)
  ) => Renderchunks (Tagged pos prox (a # b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (a :#: b))
    = renderchunks (Tagged a :: Tagged pos prox a b)
    <> renderchunks (Tagged b :: Tagged (CountContent a + pos) prox b nex)

instance {-# OVERLAPPING #-}
  ( Renderchunks (Tagged (pos+1) prox (a > b) nex)
  , KnownSymbol (Index pos prox)
  , a ?> b
  ) => Renderchunks (Tagged pos prox (a :> b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (WithAttributes a b))
    = convert (Proxy :: Proxy (Index pos prox))
    : convert a
    : renderchunks (Tagged (Child b) :: Tagged (pos+1) prox (a > b) nex)

instance
  ( Renderchunks (Tagged pos prox b (Close a))
  ) => Renderchunks (Tagged pos prox (a > b) nex) where
  {-# INLINE renderchunks #-}
  renderchunks ~(Tagged (Child b))
    = renderchunks (Tagged b :: Tagged pos prox b (Close a))

instance
  ( Renderchunks (Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # nex)))))))) (a :> b) nex)
  , KnownSymbol (Last' (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # nex)))))))))
  , KnownSymbol (Index pos prox)
  ) => Renderchunks (Tagged pos prox [a :> b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (Index pos prox))
    : map
       (\x ->
          render'
          (Tagged x :: Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # nex)))))))) (a :> b) nex)
       ) xs

instance
  ( Renderchunks (Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # nex)))))))) (a > b) nex)
  , KnownSymbol (Last' (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # nex)))))))))
  , KnownSymbol (Index pos prox)
  ) => Renderchunks (Tagged pos prox [a > b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (Index pos prox))
    : map
       (\x -> render'
          (Tagged x :: Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # nex)))))))) (a > b) nex)
       ) xs

instance
  ( Renderchunks (Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # nex)))))))) (a # b) nex)
  , KnownSymbol (Last' (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # nex)))))))))
  , KnownSymbol (Index pos prox)
  ) => Renderchunks (Tagged pos prox [a # b] nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged xs)
    = convert (undefined :: Proxy (Index pos prox))
    : map
       ( \x -> render'
         (Tagged x :: Tagged 0 (SymbolsToList (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # nex)))))))) (a # b) nex)
       ) xs

{-# RULES
"fromString'/builder"        fromString' = TLB.fromLazyText . LT.pack
"fromString'/id"             fromString' = id
"fromString'/fromString" [1] fromString' = fromString
  #-}

{-# NOINLINE fromString' #-}
fromString' :: IsString a => String -> a
fromString' = fromString

{-# RULES
"convert/a/a"                   forall f. convert' f = id
"convert/string/builder"        forall f. convert' f = TLB.fromLazyText . LT.pack
"convert/lazy text/builder"     forall f. convert' f = TLB.fromLazyText
"convert/strict text/builder"   forall f. convert' f = TLB.fromText
"convert/builder/lazy text"     forall f. convert' f = TLB.toLazyText
"convert/lazy text/strict text" forall f. convert' f = LT.toStrict
"convert/strict text/lazy text" forall f. convert' f = LT.fromStrict
"convert/inline" [1]            forall f. convert' f = f
  #-}

{-# NOINLINE convert' #-}
convert' :: a -> a
convert' = id

-- | Convert something to a target stringlike thing.
class Convert a where
  convert :: (IsString b, Monoid b) => a -> b

instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convert #-}
  convert = fromString' . symbolVal

instance Convert a => Convert (Maybe a) where
  {-# INLINE convert #-}
  convert Nothing = ""
  convert (Just x) = convert x

instance Convert Attributes where
  {-# INLINE convert #-}
  convert ~(Attributes xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

instance Convert String where
  {-# INLINE convert #-}
  convert = convert' fromString

instance Convert T.Text where
  {-# INLINE convert #-}
  convert = convert' (fromString . T.unpack)

instance Convert LT.Text where
  {-# INLINE convert #-}
  convert = convert' (fromString . LT.unpack)

instance Convert TLB.Builder where
  {-# INLINE convert #-}
  convert = convert' (fromString . LT.unpack . TLB.toLazyText)

instance Convert Int where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

instance Convert Integer where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

instance Convert Float where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

instance Convert Double where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

instance Convert Word where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

-- | Orphan show instances to faciliate ghci development.
instance Document (a # b)  => Show (a # b)  where show = render
instance Document (a > b)  => Show (a > b)  where show = render
instance Document (a :> b) => Show (a :> b) where show = render
