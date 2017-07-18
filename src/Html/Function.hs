{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-# LANGUAGE MultiParamTypeClasses #-}
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
render :: Render a b => a -> b
render = mconcat . renderList

-- | Type level replicate.  This function is quite costly to compile
-- for large Nats.  It is a bit more efficient than the conventional
-- replicate which you should use instead in non performance
-- critical code.
--
-- >>> render (replicateH (Proxy :: Proxy 2) (div_ "a")) :: String
-- "<div>a</div><div>a</div>"
class Replicate n x where
  replicateH :: Proxy n -> x -> Rep n x

instance {-# OVERLAPPING #-} Replicate 1 x where
  replicateH _ x = x

instance
  ( Replicate (n-1) x
  , Rep n x ~ (x # Rep (n-1) x)
  )
  => Replicate n x where
  replicateH _ x = x # replicateH (Proxy :: Proxy (n-1)) x

  -------------------
  -- internal code --
  -------------------

-- | Render a html document to a stream.
{-# NOINLINE renderList #-}
renderList :: Render a b => a -> [b]
renderList = renderList_

-- | Specialization for lazy builders.
{-# RULES
"renderList/renderListB"     renderList = renderListB
"renderList/renderList_" [2] renderList = renderList_
"taggedRenderList/taggedRenderListB" taggedRenderList = taggedRenderListB
"taggedRenderList/taggedRenderList_" [2] taggedRenderList = taggedRenderList_
  #-}

{-# INLINE renderListB #-}
renderListB :: forall a. Render a TLB.Builder => a -> [TLB.Builder]
renderListB x =
    augment (\c n -> foldr2 (zipWithB c (<>)) n elements contents) closing
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = listProxies (undefined :: Init (Fuse (RenderTags (PruneTags (ToTypeList a)))))
        closing  = listProxies (undefined :: Last (Fuse (RenderTags (PruneTags (ToTypeList a)))))

-- | Standard implementation for 'renderList'.
{-# INLINE renderList_ #-}
renderList_ :: forall a b. Render a b => a -> [b]
renderList_ x = g elements contents
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = listProxies (undefined :: Fuse (RenderTags (PruneTags (ToTypeList a))))
        g (a:as) (b:bs) = a:b:g as bs
        g as [] = as
        g [] bs = bs

{-# NOINLINE taggedRenderList #-}
taggedRenderList :: forall a b n.
  ( Render a b
  , ListProxies       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
  , ListProxies (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  , ListProxies (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  )
  => Tagged a n -> [b]
taggedRenderList = taggedRenderList_

{-# INLINE taggedRenderListB #-}
taggedRenderListB
  :: forall a n.
  ( Render a TLB.Builder
  , ListProxies       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
  , ListProxies (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  , ListProxies (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  )
  => Tagged a n -> [TLB.Builder]
taggedRenderListB (Tagged x) =
    augment (\c n -> foldr2 (zipWithB c (<>)) n elements contents) closing
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = listProxies (undefined :: Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
        closing  = listProxies (undefined :: Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))

{-# INLINE taggedRenderList_ #-}
taggedRenderList_
  :: forall a b n.
  ( Render a b
  , ListProxies       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
  , ListProxies (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  , ListProxies (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n))))))))
  )
  => Tagged a n -> [b]
taggedRenderList_ (Tagged x) = g elements contents
  where contents = toValueList (Tagged x :: Tagged a ())
        elements = listProxies (undefined :: Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a] # n)))))))
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

-- (?) :: (b ?> c) => (a -> b > c) -> [(String, String)] -> (a -> b > c)
-- f ? xs = addAttributes xs . f
-- infixr 9 ?

-- addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a > b)
-- addAttributes xs (Child b) = WithAttributes xs b
-- addAttributes xs (WithAttributes xs0 b) = WithAttributes (xs0 ++ xs) b

-- | Retrieve a type level list of tags and reify them as a list of strings.
class ListProxies a where
  listProxies :: (IsString b, Monoid b) => a -> [b]

instance (KnownSymbol a, ListProxies b) => ListProxies (Proxy a, b) where
  {-# INLINE listProxies #-}
  listProxies _ = convert (Proxy :: Proxy a):listProxies (undefined :: b)

instance KnownSymbol a => ListProxies (Proxy a) where
  {-# INLINE listProxies #-}
  listProxies _ = [convert (Proxy :: Proxy a)]

-- | Convert a html tree into a list of rendered content (without tags).
class ToValueList a b where
  toValueList :: (IsString b, Monoid b) => a -> [b]

instance KnownSymbol a => ToValueList (Tagged (Proxy a) n) str where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance KnownSymbol a => ToValueList (Tagged [Proxy a] n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap renderList xs]

instance {-# OVERLAPPABLE #-} Convert a str => ToValueList (Tagged a n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged x) = [convert x]

instance ToValueList (Tagged () n) str where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance
  ( ToValueList (Tagged a b) str
  , ToValueList (Tagged b n) str
  )
  => ToValueList (Tagged (a # b) n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged ~(a :#: b)) = toValueList (Tagged a :: Tagged a b) ++ toValueList (Tagged b :: Tagged b n)

instance {-# OVERLAPPING #-} ToValueList (Tagged (a > ()) n) str where
  {-# INLINE toValueList #-}
  toValueList _ = []

instance ToValueList (Tagged b (Close a)) str => ToValueList (Tagged (a > b) n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged ~(Child b)) = toValueList (Tagged b :: Tagged b (Close a))

instance
  ( ListProxies       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n)))))))
  , ListProxies (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n))))))))
  , ListProxies (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a > b] # n))))))))
  , Render (a > b) str
  ) => ToValueList (Tagged [a > b] n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap (taggedRenderList . (Tagged :: (a > b) -> Tagged (a > b) n)) xs]

instance
  ( ListProxies       (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n)))))))
  , ListProxies (Init (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n))))))))
  , ListProxies (Last (Fuse (RenderTags (Unlist (Head' (PruneTags (ToTypeList ([a # b] # n))))))))
  , Render (a # b) str)
  => ToValueList (Tagged [a # b] n) str where
  {-# INLINE toValueList #-}
  toValueList (Tagged xs) = [mconcat $ concatMap (taggedRenderList . (Tagged :: (a # b) -> Tagged (a # b) n)) xs]

-- | Convert something to a target stringlike thing.
class Convert a b where
  convert :: IsString b => a -> b

instance KnownSymbol a => Convert (Proxy a) b where
  {-# INLINE convert #-}
  convert = fromString' . symbolVal

{-# NOINLINE fromString' #-}
fromString' :: IsString a => String -> a
fromString' = fromString

{-# RULES
"fromString'/builder"        fromString' = TLB.fromLazyText . LT.pack
"fromString'/fromString" [1] fromString' = fromString
  #-}

instance Convert a b => Convert (Maybe a) b where
  {-# INLINE convert #-}
  convert Nothing = ""
  convert (Just x) = convert x

instance Convert Attribute b where
  {-# INLINE convert #-}
  convert (Attribute xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

instance {-# OVERLAPPING #-} Convert String String where convert = id
instance {-# OVERLAPPING #-} Convert String TLB.Builder where
  {-# INLINE convert #-}
  convert = TLB.fromLazyText . LT.pack
instance Convert String a where
  {-# INLINE convert #-}
  convert = fromString

instance {-# OVERLAPPING #-} Convert T.Text T.Text where convert = id
instance Convert T.Text a where convert = fromString . T.unpack

instance {-# OVERLAPPING #-} Convert LT.Text LT.Text where convert = id
instance Convert LT.Text a where convert = fromString . LT.unpack

instance {-# OVERLAPPING #-} Convert TLB.Builder TLB.Builder where convert = id
instance Convert TLB.Builder a where convert = fromString . LT.unpack . TLB.toLazyText

instance {-# OVERLAPPING #-} Convert BS8.ByteString BS8.ByteString where convert = id
instance Convert BS8.ByteString a where convert = fromString . BS8.unpack

instance {-# OVERLAPPING #-} Convert LBS8.ByteString LBS8.ByteString where convert = id
instance Convert LBS8.ByteString a where
  convert = fromString . LBS8.unpack

instance {-# OVERLAPPABLE #-} Show a => Convert a b where
  {-# INLINE convert #-}
  convert = fromString . show

-- | Orphan show instances to faciliate ghci development.
instance Render (a # b) String => Show (a # b) where
  show = render

instance Render (a > b) String => Show (a > b) where
  show = render

-- | Constraint for renderable html trees.
--
-- It must be possible to
-- * convert the html tree into a list of proxies representing the tags
-- * inspect the last element of this list
-- * inspect the init of this list
-- * flatten the content to a list
-- * convert strings to the resulting type
-- * mappend and mempty the resulting type
type Render html string
  = ( -- Type level
      ListProxies       (Fuse (RenderTags (PruneTags (ToTypeList html))))
    , ListProxies (Init (Fuse (RenderTags (PruneTags (ToTypeList html)))))
    , ListProxies (Last (Fuse (RenderTags (PruneTags (ToTypeList html)))))

      -- Value level
    , ToValueList (Tagged html ()) string
    , IsString string
    , Monoid string
    )
