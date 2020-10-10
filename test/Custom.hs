{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Custom where

import Html

import qualified Html.Attribute as A

hxPost_ :: a -> 'CustomA "hx-post" := a
hxPost_ = A.custom_
