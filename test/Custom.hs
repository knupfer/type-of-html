{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}

module Custom where

import Html

data instance Attribute "hx-post" 'True 'False = HxPostA
