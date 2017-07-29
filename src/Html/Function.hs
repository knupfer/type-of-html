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
import Data.Semigroup (Semigroup(..))

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- | Render a html document.  The resulting type can be a String,
-- strict Text, lazy Text, Builder, ByteString or lazy ByteString.
-- For performance it is recommended to use a lazy Text or a Builder.
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
render ::
  ( Document a
  , Monoid b
  , IsString b
  ) => a -> b
render = mconcat . renderList

  -------------------
  -- internal code --
  -------------------

-- | Hide internal type for haddock.
type Document a =
  ( Reify (Symbols a)
  , Reify (Last (Symbols a))
  , Reify (Init (Symbols a))
  , ToValueList (Tagged a ())
  )

-- | Render a html document to a stream.
{-# NOINLINE renderList #-}
renderList ::
  ( Monoid b
  , IsString b
  , ToValueList (Tagged a ())
  , Reify (Symbols a)
  , Reify (Last (Symbols a))
  , Reify (Init (Symbols a))
  ) => a -> [b]
renderList = renderList_

-- | Specialization for lazy builders.
{-# RULES
"renderList/renderListB"     renderList = renderListB
"renderList/renderList_" [2] renderList = renderList_
"taggedRenderList/taggedRenderListB" taggedRenderList = taggedRenderListB
"taggedRenderList/taggedRenderList_" [2] taggedRenderList = taggedRenderList_
  #-}

{-# INLINE renderListB #-}
renderListB :: forall a.
  ( Reify (Last (Symbols a))
  , Reify (Init (Symbols a))
  , ToValueList (Tagged a ())
  ) => a -> [TLB.Builder]
renderListB x =
    augment (\c n -> foldr2 (zipWithB c (<>)) n elements contents) closing
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = reify (undefined :: Init (Symbols a))
        closing  = reify (undefined :: Last (Symbols a))

-- | Standard implementation for 'renderList'.
{-# INLINE renderList_ #-}
renderList_ :: forall a b.
  ( Reify (Symbols a)
  , ToValueList (Tagged a ())
  , Monoid b
  , IsString b
  ) => a -> [b]
renderList_ x = g elements contents
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = reify (undefined :: Symbols a)
        g (a:as) (b:bs) = a:b:g as bs
        g as [] = as
        g [] bs = bs

{-# NOINLINE taggedRenderList #-}
taggedRenderList :: forall a b n.
  ( IsString b
  , Monoid b
  , ToValueList (Tagged a ())
  , Reify       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
  , Reify (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  , Reify (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  )
  => Tagged a n -> [b]
taggedRenderList = taggedRenderList_

{-# INLINE taggedRenderListB #-}
taggedRenderListB
  :: forall a n.
  ( ToValueList (Tagged a ())
  , Reify (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  , Reify (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  )
  => Tagged a n -> [TLB.Builder]
taggedRenderListB (Tagged x) =
    augment (\c n -> foldr2 (zipWithB c (<>)) n elements contents) closing
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = reify (undefined :: Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
        closing  = reify (undefined :: Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))

{-# INLINE taggedRenderList_ #-}
taggedRenderList_
  :: forall a b n.
  ( IsString b
  , Monoid b
  , ToValueList (Tagged a ())
  , Reify (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
  )
  => Tagged a n -> [b]
taggedRenderList_ (Tagged x) = g elements contents
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = reify (undefined :: Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
        g (a:as) (b:bs) = a:b:g as bs
        g as [] = as
        g [] bs = bs

type family Unlist xs where
  Unlist [x] = x

-- | Fuseable builder zip.
{-# INLINE [0] zipWithB #-}
zipWithB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithB c f x y r = (x `f` y) `c` r

-- | Fuseable fold with two lists.
{-# INLINE [0] foldr2 #-}
foldr2 :: Monoid a => (a -> a -> c -> c) -> c -> [a] -> [a] -> c
foldr2 k z = go
  where go []     _      = z
        go _      []     = z
        go (x:xs) (y:ys) = k x y (go xs ys)

-- | Left fold optimization.
foldr2l :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2l _ z _ _ []     = z
foldr2l k _ x r (y:ys) = k x y (r ys)

{-# RULES
"foldr2/foldr2l" forall k z ys (g :: forall b. (a -> b -> b) -> b -> b).
                 foldr2 k z (build g) ys = g (foldr2l k z) (const z) ys
  #-}

addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a :> b)
addAttributes xs (Child b) = WithAttributes (Attributes xs) b

-- | Retrieve a type level list of tags and reify them as a list of strings.
class Reify a where
  reify :: (IsString b, Monoid b) => a -> [b]

instance (KnownSymbol a, Reify b) => Reify (Proxy a, b) where
  {-# INLINE reify #-}
  reify _ = convert (Proxy :: Proxy a):reify (undefined :: b)

instance KnownSymbol a => Reify (Proxy a) where
  {-# INLINE reify #-}
  reify _ = [convert (Proxy :: Proxy a)]

-- | Convert a html tree into a list of rendered content (without tags).
class ToValueList a where
  toValueList :: (IsString b, Monoid b) => a -> [b]

instance KnownSymbol a => ToValueList (Tagged (Proxy a) n) where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance KnownSymbol a => ToValueList (Tagged [Proxy a] n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap renderList xs]

instance {-# OVERLAPPABLE #-} Convert a => ToValueList (Tagged a n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged x) = [convert x]

instance ToValueList (Tagged () n) where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance
  ( ToValueList (Tagged a b)
  , ToValueList (Tagged b n)
  )
  => ToValueList (Tagged (a # b) n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged ~(a :#: b)) = toValueList (Tagged a :: Tagged a b) ++ toValueList (Tagged b :: Tagged b n)

instance {-# OVERLAPPING #-} ToValueList (Tagged (a > ()) n) where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance {-# OVERLAPPING #-} (ToValueList (Tagged (a > b) n)) => ToValueList (Tagged (a :> b) n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged (WithAttributes a b)) = convert a : toValueList (Tagged (Child b) :: Tagged (a > b) n)

instance ToValueList (Tagged b (Close a)) => ToValueList (Tagged (a > b) n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged ~(Child b)) = toValueList (Tagged b :: Tagged b (Close a))

instance
  ( Reify       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # n)))))))
  , Reify (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # n))))))))
  , Reify (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a :> b] # n))))))))
  , ToValueList (Tagged (a :> b) ())
  ) => ToValueList (Tagged [a :> b] n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap (taggedRenderList . (Tagged :: (a :> b) -> Tagged (a :> b) n)) xs]

instance
  ( Reify       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n)))))))
  , Reify (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n))))))))
  , Reify (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n))))))))
  , ToValueList (Tagged (a > b) ())
  ) => ToValueList (Tagged [a > b] n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap (taggedRenderList . (Tagged :: (a > b) -> Tagged (a > b) n)) xs]

instance
  ( Reify       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n)))))))
  , Reify (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n))))))))
  , Reify (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n))))))))
  , ToValueList (Tagged (a # b) ())
  ) => ToValueList (Tagged [a # b] n) where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap (taggedRenderList . (Tagged :: (a # b) -> Tagged (a # b) n)) xs]

-- | Convert something to a target stringlike thing.
class Convert a where
  convert :: IsString b => a -> b

instance KnownSymbol a => Convert (Proxy a) where
  {-# INLINE convert #-}
  convert = fromString' . symbolVal

{-# NOINLINE fromString' #-}
fromString' :: IsString a => String -> a
fromString' = fromString

{-# NOINLINE convert' #-}
convert' :: a -> a
convert' = id

{-# RULES
"fromString'/builder"        fromString' = TLB.fromLazyText . LT.pack
"fromString'/id"             fromString' = id
"fromString'/fromString" [1] fromString' = fromString
  #-}

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

instance Convert a => Convert (Maybe a) where
  {-# INLINE convert #-}
  convert Nothing = ""
  convert (Just x) = convert x

instance Convert Attributes where
  {-# INLINE convert #-}
  convert (Attributes xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

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

instance Convert BS8.ByteString where
  {-# INLINE convert #-}
  convert = convert' (fromString . BS8.unpack)

instance Convert LBS8.ByteString where
  {-# INLINE convert #-}
  convert = convert' (fromString . LBS8.unpack)

instance {-# OVERLAPPABLE #-} Show a => Convert a where
  {-# INLINE convert #-}
  convert = convert' (fromString . show)

-- | Orphan show instances to faciliate ghci development.
instance
  ( Reify (Symbols (a # b))
  , Reify (Last (Symbols (a # b)))
  , Reify (Init (Symbols (a # b)))
  , ToValueList (Tagged (a # b) ())
  ) => Show (a # b) where
  show = render

instance
  ( Reify (Symbols (a > b))
  , Reify (Last (Symbols (a > b)))
  , Reify (Init (Symbols (a > b)))
  , ToValueList (Tagged (a > b) ())
  ) => Show (a > b) where
  show = render

instance
  ( Reify (Symbols (a :> b))
  , Reify (Last (Symbols (a :> b)))
  , Reify (Init (Symbols (a :> b)))
  , ToValueList (Tagged (a :> b) ())
  ) => Show (a :> b) where
  show = render
