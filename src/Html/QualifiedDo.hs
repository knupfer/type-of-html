{-# LANGUAGE TypeOperators #-}

-- | Enable blaze-like notation using the [@QualifiedDo@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/qualified_do.html#extension-QualifiedDo) language extension.
module Html.QualifiedDo where

import Html.Type

(>>) :: a -> b -> a # b
a >> b = a # b
{-# INLINE (>>) #-}
