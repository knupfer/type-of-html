{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Small where

import Html
import qualified Html.Attribute as A

import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.IORef
import System.IO.Unsafe

import Criterion.Main
import Control.Arrow

small :: Benchmark
small = bgroup "Small"
  [ bench "oneElement"                   $ nf (renderByteString . oneElement) ""
  , bench "oneElement'"                  $ nf (renderByteString . oneElement') ""
  , bench "oneElement''"                 $ nf (renderByteString . oneElement'') ""
  , bench "nestedElement"                $ nf (renderByteString . nestedElement) ""
  , bench "nestedElement'"               $ nf (renderByteString . nestedElement') ""
  , bench "nestedElement''"              $ nf (renderByteString . nestedElement'') ""
  , bench "parallelElement"              $ nf (renderByteString . parallelElement) ""
  , bench "parallelElement'"             $ nf (renderByteString . parallelElement') ""
  , bench "parallelElement''"            $ nf (renderByteString . parallelElement'') ""
  , bench "listElement"                  $ nf (renderByteString . listElement) ""
  , bench "listElement'"                 $ nf (renderByteString . listElement') ""
  , bench "listElement''"                $ nf (renderByteString . listElement'') ""
  , bench "Int"                          $ nf renderByteString (123456789 :: Int)
  , bench "Integer"                      $ nf renderByteString (123456789 :: Integer)
  , bench "Double"                       $ nf renderByteString (123456789 :: Double)
  , bench "Float"                        $ nf renderByteString (123456789 :: Float)
  , bench "Word"                         $ nf renderByteString (123456789 :: Word)
  , bench "Proxy"                        $ nf renderByteString (Proxy :: Proxy "abc")
  , bench "Char"                         $ nf renderByteString 'a'
  , bench "oneElement Proxy"             $ nf (renderByteString . oneElement) (Proxy :: Proxy "abc")
  , bench "()"                           $ nf renderByteString ()
  , bench "oneElement ()"                $ nf (renderByteString . oneElement) ()
  , bench "oneAttribute"                 $ nf (renderByteString . A.class_) ""
  , bench "oneAttribute ()"              $ nf (renderByteString . A.class_)  ()
  , bench "oneAttribute Proxy"           $ nf (renderByteString . A.class_)  (Proxy :: Proxy "abc")
  , bench "parallelAttribute"            $ nf (\x -> renderByteString $ A.class_ x # A.id_ x) ""
  , bench "elementWithAttribute"         $ nf (\x -> renderByteString $ div_A (A.class_ x) x) ""
  , bench "elementWithParallelAttribute" $ nf (\x -> renderByteString $ div_A (A.class_ x # A.id_ x) x) ""
  , bench "listOfAttributes"             $ nf (\x -> renderByteString [A.class_ x, A.class_ x]) ""
  , bench "Runtime String"               $ nfIO (renderByteString <$> readIORef runtimeString)
  , bench "String"                       $ nf renderByteString "abcdefghijklmnopqrstuvwxyz<>&;"
  , bench "Runtime Raw String"           $ nfIO (renderByteString . Raw <$> readIORef runtimeString)
  , bench "Raw String"                   $ nf renderByteString (Raw "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime strict Text"          $ nfIO (renderByteString <$> readIORef runtimeStrictText)
  , bench "strict Text"                  $ nf renderByteString (T.pack "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime Raw strict Text"      $ nfIO (renderByteString . Raw <$> readIORef runtimeStrictText)
  , bench "Raw strict Text"              $ nf renderByteString (Raw (T.pack "abcdefghijklmnopqrstuvwxyz<>&;"))
  , bench "Runtime lazy Text"            $ nfIO (renderByteString <$> readIORef runtimeLazyText)
  , bench "lazy Text"                    $ nf renderByteString (LT.pack "abcdefghijklmnopqrstuvwxyz<>&;")
  , bench "Runtime Raw lazy Text"        $ nfIO (renderByteString . Raw <$> readIORef runtimeLazyText)
  , bench "Raw lazy Text"                $ nf renderByteString (Raw (LT.pack "abcdefghijklmnopqrstuvwxyz<>&;"))
  , bench "listOfListOf"                 $ nf (\x -> renderByteString $ div_ [i_ [span_ x]]) ""
  ]
  where

{-# NOINLINE runtimeString #-}
runtimeString :: IORef String
runtimeString = unsafePerformIO $ newIORef "abcdefghijklmnopqrstuvwxyz<>&;"

{-# NOINLINE runtimeLazyText #-}
runtimeLazyText :: IORef LT.Text
runtimeLazyText = unsafePerformIO . newIORef $ LT.pack "abcdefghijklmnopqrstuvwxyz<>&;"

{-# NOINLINE runtimeStrictText #-}
runtimeStrictText :: IORef T.Text
runtimeStrictText = unsafePerformIO . newIORef $ T.pack "abcdefghijklmnopqrstuvwxyz<>&;"

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

