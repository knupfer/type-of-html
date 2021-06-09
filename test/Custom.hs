{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}

module Custom where

import Html

newtype instance Attribute "hx-post" 'True v = HxPostA v
