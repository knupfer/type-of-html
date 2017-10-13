{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html
import qualified Html.Attribute as A

import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Criterion.Main
import Control.Arrow

small :: Benchmark
small = bgroup "Small"
  [ bench "oneElement"                   $ r oneElement ""
  , bench "oneElement'"                  $ r oneElement' ""
  , bench "oneElement''"                 $ r oneElement'' ""
  , bench "nestedElement"                $ r nestedElement ""
  , bench "nestedElement'"               $ r nestedElement' ""
  , bench "nestedElement''"              $ r nestedElement'' ""
  , bench "parallelElement"              $ r parallelElement ""
  , bench "parallelElement'"             $ r parallelElement' ""
  , bench "parallelElement''"            $ r parallelElement'' ""
  , bench "listElement"                  $ r listElement ""
  , bench "listElement'"                 $ r listElement' ""
  , bench "listElement''"                $ r listElement'' ""
  , bench "Int"                          $ r id (123456789 :: Int)
  , bench "Integer"                      $ r id (123456789 :: Integer)
  , bench "Double"                       $ r id (123456789 :: Double)
  , bench "Float"                        $ r id (123456789 :: Float)
  , bench "Word"                         $ r id (123456789 :: Word)
  , bench "Proxy"                        $ r id (Proxy :: Proxy "abc")
  , bench "Char"                         $ r id 'a'
  , bench "oneElement Proxy"             $ r oneElement (Proxy :: Proxy "abc")
  , bench "()"                           $ r id ()
  , bench "oneElement ()"                $ r oneElement ()
  , bench "oneAttribute"                 $ r A.class_ ""
  , bench "oneAttribute ()"              $ r A.class_ ()
  , bench "oneAttribute Proxy"           $ r A.class_ (Proxy :: Proxy "abc")
  , bench "parallelAttribute"            $ r (\x -> A.class_ x # A.id_ x) ""
  , bench "elementWithAttribute"         $ r (\x -> div_A (A.class_ x) x) ""
  , bench "elementWithParallelAttribute" $ r (\x -> div_A (A.class_ x # A.id_ x) x) ""
  , bench "listOfAttributes"             $ r (\x -> [A.class_ x, A.class_ x]) ""
  , bench "Runtime String"               $ r id runtimeTxt
  , bench "String"                       $ r id "abcdefghijklmnopqrstuvwxyz<>&;"
  , bench "Runtime Raw String"           $ r id (Raw runtimeTxt)
  , bench "Raw String"                   $ r id (Raw "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime strict Text"          $ r id (T.pack runtimeTxt)
  , bench "strict Text"                  $ r id (T.pack "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime Raw strict Text"      $ r id (Raw (T.pack runtimeTxt))
  , bench "Raw strict Text"              $ r id (Raw (T.pack "abcdefghijklmnopqrstuvwxyz<>&;"))
  , bench "Runtime lazy Text"            $ r id (LT.pack runtimeTxt)
  , bench "lazy Text"                    $ r id (LT.pack "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime Raw lazy Text"        $ r id (Raw (LT.pack runtimeTxt))
  , bench "Raw lazy Text"                $ r id (Raw (LT.pack "abcdefghijklmnopqrstuvwxyz<>&;"))
  , bench "listOfListOf"                 $ r (\x -> div_ [i_ [span_ x]]) ""
  ]
  where r f x = nf (renderByteString . f) x
        runtimeTxt = reverse "abcdefghijklmnopqrstuvwxyz<>&;"

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

