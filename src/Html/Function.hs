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

class Replicate n x where
  -- | Type level replicate.  This function is quite costly to compile
  -- for large Nats.  It is a bit more efficient than the conventional
  -- replicate which you should use instead in non performance
  -- critical code.
  --
  -- >>> render (replicateH (Proxy :: Proxy 2) (div_ "a")) :: String
  -- "<div>a</div><div>a</div>"
  replicateH :: Proxy n -> x -> Rep n x

instance {-# OVERLAPPING #-} Replicate 1 x where
  replicateH _ x = x

instance (Replicate (n-1) x, Rep n x ~ (x # Rep (n-1) x)) => Replicate n x where
  replicateH _ x = x # replicateH (Proxy :: Proxy (n-1)) x

  -------------------
  -- internal code --
  -------------------

{-# NOINLINE renderList #-}
renderList :: Render a b => a -> [b]
renderList = renderList_

{-# INLINE renderListB #-}
renderListB :: Render a TLB.Builder => a -> [TLB.Builder]
renderListB x =
    augment (\c n -> foldr2_ (zipWithFB_ c (<>)) n elements contents) closing
  where contents = flatR x
        elements = listProxies (f x)
        closing = listProxies (g x)
        f :: x -> Init (Fuse (RenderRecursive (PruneTags (Flatten x))))
        f = undefined
        g :: x -> Last (Fuse (RenderRecursive (PruneTags (Flatten x))))
        g = undefined

{-# INLINE renderList_ #-}
renderList_ :: Render a b => a -> [b]
renderList_ x = g elements contents
  where contents = flatR x
        elements = listProxies (f x)
        f :: x -> Fuse (RenderRecursive (PruneTags (Flatten x)))
        f = undefined
        g (a:as) (b:bs) = a:b:g as bs
        g as [] = as
        g [] bs = bs

{-# INLINE renderListLT #-}
renderListLT :: Render a LT.Text => a -> [LT.Text]
renderListLT = renderList_

{-# INLINE renderListT #-}
renderListT :: Render a T.Text => a -> [T.Text]
renderListT = renderList_

{-# INLINE renderListS #-}
renderListS :: Render a String => a -> [String]
renderListS = renderList_

{-# RULES
"renderList/builder"     renderList = renderListB
"renderList/lazy text"   renderList = renderListLT
"renderList/strict text" renderList = renderListT
"renderList/string"      renderList = renderListS
  #-}

{-# INLINE [0] zipWithFB_ #-}
zipWithFB_ :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB_ c f = \x y r -> (x `f` y) `c` r

{-# INLINE [0] foldr2_ #-}
foldr2_ :: Monoid a => (a -> a -> c -> c) -> c -> [a] -> [a] -> c
foldr2_ k z = go
  where
        go []     _      = z
        go _      []     = z
        go (x:xs) (y:ys) = k x y (go xs ys)

foldr2_left_ :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left_ _k  z _x _r []     = z
foldr2_left_  k _z  x  r (y:ys) = k x y (r ys)

{-# RULES
"foldr2/left_"   forall k z ys (g:: forall b.(a->b->b)->b->b) .
                  foldr2_ k z (build g) ys = g (foldr2_left_  k z) (\_ -> z) ys
 #-}

--(?) :: (b ?> c) => (a -> b > c) -> [(String, String)] -> (a -> b > c)
--f ? xs = addAttributes xs . f
--infixr 9 ?


-- addAttributes :: (a ?> b) => [(String, String)] -> (a > b) -> (a > b)
-- addAttributes xs (Child b) = WithAttributes xs b
-- addAttributes xs (WithAttributes xs0 b) = WithAttributes (xs0 ++ xs) b


class ListProxies a b where
  listProxies :: (IsString b, Monoid b) => a -> [b]

instance (KnownSymbol a, ListProxies b str) => ListProxies (Proxy a, b) str where
  {-# INLINE listProxies #-}
  listProxies _ = doRender (Proxy :: Proxy a):listProxies (undefined :: b)

instance KnownSymbol a => ListProxies (Proxy a) str where
  {-# INLINE listProxies #-}
  listProxies _ = [doRender (Proxy :: Proxy a)]

class FlatR a b where
  flatR :: (IsString b, Monoid b) => a -> [b]

instance KnownSymbol a => FlatR (Proxy a) str where
  {-# INLINE flatR #-}
  flatR _ = []

instance KnownSymbol a => FlatR [Proxy a] str where
  {-# INLINE flatR #-}
  flatR xs = [mconcat $ concatMap renderList xs]

instance {-# OVERLAPPABLE #-} DoRender a str => FlatR a str where
  {-# INLINE flatR #-}
  flatR x = [doRender x]

instance FlatR () str where
  {-# INLINE flatR #-}
  flatR _ = []

instance (FlatR a str, FlatR b str) => FlatR (a # b) str where
  {-# INLINE flatR #-}
  flatR ~(a :#: b) = flatR a ++ flatR b

instance {-# OVERLAPPING #-} FlatR (a > ()) str where
  {-# INLINE flatR #-}
  flatR _ = []

instance FlatR b str => FlatR (a > b) str where
  {-# INLINE flatR #-}
  flatR ~(Child x) = flatR x

instance (Render (a > b) str) => FlatR [a > b] str where
  {-# INLINE flatR #-}
  flatR xs = [mconcat $ concatMap renderList xs]

instance (Render (a # b) str) => FlatR [a # b] str where
  {-# INLINE flatR #-}
  flatR xs = [mconcat $ concatMap renderList xs]

class DoRender a b where
  doRender :: IsString b => a -> b

instance KnownSymbol a => DoRender (Proxy a) b where
  {-# INLINE doRender #-}
  doRender = fromString' . symbolVal

{-# NOINLINE fromString' #-}
fromString' :: IsString a => String -> a
fromString' = fromString

{-# RULES
"fromString'/builder" fromString' = TLB.fromLazyText . LT.pack
"fromString/fromString" [10] fromString' = fromString
#-}


instance DoRender a b => DoRender (Maybe a) b where
  {-# INLINE doRender #-}
  doRender Nothing = ""
  doRender (Just x) = doRender x

instance DoRender Attribute b where
  {-# INLINE doRender #-}
  doRender (Attribute xs) = fromString $ concat [ ' ' : a ++ "=" ++ b | (a,b) <- xs]

instance {-# OVERLAPPING #-} DoRender String String where doRender = id
instance DoRender String a where doRender = fromString

instance {-# OVERLAPPING #-} DoRender T.Text T.Text where doRender = id
instance DoRender T.Text a where doRender = fromString . T.unpack

instance {-# OVERLAPPING #-} DoRender LT.Text LT.Text where doRender = id
instance DoRender LT.Text a where doRender = fromString . LT.unpack

instance {-# OVERLAPPING #-} DoRender TLB.Builder TLB.Builder where doRender = id
instance DoRender TLB.Builder a where doRender = fromString . LT.unpack . TLB.toLazyText

instance {-# OVERLAPPING #-} DoRender BS8.ByteString BS8.ByteString where doRender = id
instance DoRender BS8.ByteString a where doRender = fromString . BS8.unpack

instance {-# OVERLAPPING #-} DoRender LBS8.ByteString LBS8.ByteString where doRender = id
instance DoRender LBS8.ByteString a where
  doRender = fromString . LBS8.unpack

instance {-# OVERLAPPABLE #-} Show a => DoRender a b where doRender = fromString . show

instance Render (a # b) String => Show (a # b) where
  show = render

instance Render (a > b) String => Show (a > b) where
  show = render

type Render html string
  = ( IsString string
    , ListProxies
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
      ) string
    , ListProxies
       ( Init
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
        )
      ) string
    , ListProxies
      ( Last
        ( Fuse
          ( RenderRecursive
            ( PruneTags
              ( Flatten html
              )
            )
          )
        )
      ) string
    , FlatR html string
    , Monoid string
    )
