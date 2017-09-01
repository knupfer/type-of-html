{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}

module Html.String where

import Html.Type
import GHC.TypeLits
import Data.Proxy
import Data.Semigroup

{-# INLINE render #-}
render :: forall a. Document a => a -> String
render x = concat $ renderchunks (Tagged x :: Tagged (Symbols a) a ())
                   <> [convert (Proxy @ (Last' (Symbols a)))]

escape :: String -> String
escape = concatMap $ \case
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '&'  -> "&amp;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  x    -> pure x

type Document a =
  ( Renderchunks (Tagged (Symbols a) a ())
  , KnownSymbol (Last' (Symbols a))
  )

class Convert a where convert :: a -> String

instance Convert String  where convert = escape
instance Convert Int     where convert = show
instance Convert Integer where convert = show
instance Convert Float   where convert = show
instance Convert Double  where convert = show
instance Convert Word    where convert = show

instance KnownSymbol a => Convert (Proxy a) where convert = symbolVal
instance Convert Attributes where
  {-# INLINE convert #-}
  convert ~(Attributes xs) = mconcat [(" " ++ a ++ "=\"") <> escape b <> "\"" | (a,b) <- xs]

class Renderchunks a where
  renderchunks :: a -> [String]

instance KnownSymbol a => Renderchunks (Tagged prox (Proxy a) nex) where renderchunks _ = mempty
instance Renderchunks (Tagged prox () nex) where renderchunks _ = mempty

instance {-# OVERLAPPABLE #-}
  ( Convert val
  , KnownSymbol (HeadL prox)
  ) => Renderchunks (Tagged prox val nex) where
  {-# INLINE renderchunks #-}
  renderchunks (Tagged x)
    = convert (Proxy @ (HeadL prox))
    : [convert x]

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
  renderchunks (Tagged ~(WithAttributes a b))
    = convert (Proxy @ (HeadL prox))
    : convert a
    : renderchunks (Tagged b :: t)

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
    = convert (Proxy @ (HeadL prox))
    : Prelude.concatMap (\x -> renderchunks (Tagged x :: t1) <> [closing]) xs
    where closing = convert (Proxy @ (Last' t2))
