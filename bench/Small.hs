{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html

import Control.Arrow

oneElement, oneElement', oneElement''
  :: ('Div ?> a)
  => a -> 'Div > a
oneElement x = div_ x
oneElement'  = div_
oneElement'' = \x -> div_ x

nestedElement, nestedElement', nestedElement''
  :: ('Span ?> a)
  => a -> 'Div > ('Span > a)
nestedElement x = div_ (span_ x)
nestedElement'  = div_ . span_
nestedElement'' = \x -> div_ (span_ x)

parallelElement, parallelElement', parallelElement''
  :: ('Span ?> a, 'Div ?> a)
  => a -> ('Div > a) # ('Span > a)
parallelElement x = div_ x # span_ x
parallelElement'  = uncurry (#) . (div_ &&& span_)
parallelElement'' = \x -> div_ x # span_ x

listElement, listElement', listElement''
  :: ('Div ?> a)
  => a -> ['Div > a]
listElement x = [div_ x]
listElement' = pure . div_
listElement'' = \x -> [div_ x]

