{-# LANGUAGE DataKinds #-}

module Custom where

import Html

hxPost :: Attribute "hx-post" 'True 'False
hxPost = CustomA
