{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP       #-}

-- | Note that the allocation numbers are only reproducible on linux using the nix shell.

module Main where

import Html
import qualified Html.Attribute as A
import qualified Small          as S
import qualified Medium         as M
import qualified Big            as B

import Weigh
import Control.DeepSeq
import Data.Proxy
import Data.Int

import System.IO.Temp
import GHC
import GHC.Paths (libdir)
import DynFlags
import Control.Monad

allocs :: Int64 -> Weight -> Maybe String
allocs n w
  | n' > n = Just $ "More" ++ answer
  | n' < n = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = weightAllocatedBytes w
        answer = " allocated bytes than expected: " ++ show (abs $ n' - n)

allocsError :: Int -> Int -> Int -> Weight -> Maybe String
allocsError i m n w
  | n' > (m'+1) = Just $ "More" ++ answer
  | n' < (m'-1) = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = round (fromIntegral (weightAllocatedBytes w) / (10^i) :: Rational) :: Int
        m' = n + m
        answer = " allocated bytes than expected: " ++ pretty (abs $ m' - n')
        pretty x = show x ++ " e" ++ show i

f :: NFData b => String -> (a -> b) -> a -> Int64 -> Weigh ()
f s g x n = validateFunc s g x (allocs n)

main :: IO ()
main = withSystemTempDirectory "compile" $ \tmp -> mainWith $ do

  --                                                                        ghc version    822   844   862
  f "()"                   renderByteString ()                                    $ ghc [   80,   48,  -32 ]
  f "Int"                  renderByteString (123456789 :: Int)                    $ ghc [  216             ]
  f "Word"                 renderByteString (123456789 :: Word)                   $ ghc [  216             ]
  f "Char"                 renderByteString 'a'                                   $ ghc [  216             ]
  f "Integer"              renderByteString (123456789 :: Integer)                $ ghc [  248             ]
  f "Proxy"                renderByteString (Proxy :: Proxy "a")                  $ ghc [  264,    0,   16 ]
  f "oneElement Proxy"     (renderByteString . S.oneElement) (Proxy :: Proxy "b") $ ghc [  264,    0,   16 ]
  f "oneElement ()"        (renderByteString . S.oneElement) ()                   $ ghc [  264,    0,   16 ]
  f "oneAttribute ()"      (renderByteString . A.class_) ()                       $ ghc [  264,    0,   16 ]
  f "oneAttribute Proxy"   (renderByteString . A.class_) (Proxy :: Proxy "c")     $ ghc [  264,    0,   16 ]
  f "listElement"          (renderByteString . S.listElement) ()                  $ ghc [  608             ]
  f "Double"               renderByteString (123456789 :: Double)                 $ ghc [  360             ]
  f "oneElement"           (renderByteString . S.oneElement) ""                   $ ghc [  368             ]
  f "nestedElement"        (renderByteString . S.nestedElement) ""                $ ghc [  368             ]
  f "listOfAttributes"     (\x -> renderByteString [A.class_ x, A.class_ x]) ()   $ ghc [  712             ]
  f "Float"                renderByteString (123456789 :: Float)                  $ ghc [  400             ]
  f "oneAttribute"         (renderByteString . A.class_) ""                       $ ghc [  520             ]
  f "parallelElement"      (renderByteString . S.parallelElement) ""              $ ghc [  520             ]
  f "parallelAttribute"    (\x -> renderByteString $ A.class_ x # A.id_ x) ""     $ ghc [  736             ]
  f "elementWithAttribute" (\x -> renderByteString $ div_A (A.class_ x) x) ""     $ ghc [  696             ]
  f "listOfListOf"         (\x -> renderByteString $ div_ [i_ [span_ x]]) ()      $ ghc [ 1200,    0,   64 ]
  f "helloWorld"           (renderByteString . M.helloWorld) ()                   $ ghc [ 1248,    0,   16 ]
  f "page"                 (renderByteString . M.page) ()                         $ ghc [ 1400,    0,   16 ]
  f "table"                (renderByteString . M.table) (2,2)                     $ ghc [ 2640,  -32,  136 ]
  f "AttrShort"            (renderByteString . M.attrShort) ()                    $ ghc [ 2616,    0,   88 ]
  f "pageA"                (renderByteString . M.pageA) ()                        $ ghc [ 2848,    0,   16 ]
  f "AttrLong"             (renderByteString . M.attrLong) ()                     $ ghc [ 2616,    0,   16 ]
  f "Big table"            (renderByteString . M.table) (15,15)                   $ ghc [54040,-1824, 1904 ]
  f "Big page"             (renderByteString . B.page) ()                         $ ghc [27832,  -56,   72 ]
  let g x y z = validateAction x (compile tmp) y . allocsError 7 z                $ ghc [  118,    0,    4 ]
  g "Compile Library"   "Html"                                                    $ ghc [    0             ]
  g "Compile Small.hs"  "Small"                                                   $ ghc [    1             ]
  g "Compile Medium.hs" "Medium"                                                  $ ghc [   39,    2,    2 ]
  g "Compile Big.hs"    "Big"                                                     $ ghc [   73,    2,    1 ]
  g "Compile Perf.hs"   "bench/Perf.hs"                                           $ ghc [  117,  213,    4 ]
  g "Compile X0.hs"     "bench/Compilation/X0.hs"                                 $ ghc [    3,    1       ]
  g "Compile X1.hs"     "bench/Compilation/X1.hs"                                 $ ghc [    4,    2,   -2 ]
  g "Compile X2.hs"     "bench/Compilation/X2.hs"                                 $ ghc [    5             ]
  g "Compile X4.hs"     "bench/Compilation/X4.hs"                                 $ ghc [    7             ]
  g "Compile X8.hs"     "bench/Compilation/X8.hs"                                 $ ghc [   12             ]
  g "Compile X16.hs"    "bench/Compilation/X16.hs"                                $ ghc [   25             ]
  g "Compile X32.hs"    "bench/Compilation/X32.hs"                                $ ghc [   64,    2       ]
  g "Compile X64.hs"    "bench/Compilation/X64.hs"                                $ ghc [  203,    4,   -4 ]
  g "Compile X128.hs"   "bench/Compilation/X128.hs"                               $ ghc [  714,    4,   -5 ]

ghc :: Num a => [a] -> a
ghc xs = sum [y | (y, v) <- zip xs supportedGhcs, v <= __GLASGOW_HASKELL__]

  where supportedGhcs = [802, 804, 806] :: [Int]

compile :: String -> String -> IO ()
compile out m =
  void . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags (dflags {optLevel = 2, importPaths = ["src", "bench"], hiDir = Just out, objectDir = Just out, outputFile = Just (out ++ "/out")})
    target <- guessTarget m Nothing
    setTargets [target]
    load LoadAllTargets
