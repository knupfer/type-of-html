{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html

oneElement
  :: ('Div ?> a)
  => a -> 'Div > a
oneElement = div_

nestedElement
  :: ('Span ?> a)
  => a -> 'Div > ('Span > a)
nestedElement x = div_ (span_ x)

parallelElement
  :: ('Span ?> a, 'Div ?> a)
  => a -> ('Div > a) # ('Span > a)
parallelElement x = div_ x # span_ x

listElement
  :: ('Div ?> a)
  => a -> ['Div > a]
listElement x = [div_ x]

