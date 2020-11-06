{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html

-- oneElement
--   :: ('Div ?> a)
--   => a -> 'Div > a
oneElement = (Div :>)

-- nestedElement
--   :: ('Span ?> a)
--   => a -> 'Div > ('Span > a)
nestedElement x = Div :> Span :> x

-- parallelElement
--   :: ('Span ?> a, 'Div ?> a)
--   => a -> ('Div > a) # ('Span > a)
parallelElement x = Div :> x # Span :> x

-- listElement
--   :: ('Div ?> a)
--   => a -> ['Div > a]
listElement x = [Div :> x]

