{-# LANGUAGE DataKinds #-}

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
        answer = " allocated bytes than " ++ commas n ++ ": " ++ commas n'

allocsError :: Int -> Int -> Weight -> Maybe String
allocsError n i w
  | n' > (n+1) = Just $ "More" ++ answer
  | n' < (n-1) = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = round (fromIntegral (weightAllocatedBytes w) / (10^i) :: Rational) :: Int
        answer = " allocated bytes than "
              ++ pretty n
              ++ ": "
              ++ pretty n'
        pretty x = show x ++ " e" ++ show i

f :: NFData b => String -> Int64 -> (a -> b) -> a -> Weigh ()
f s n g x = validateFunc s g x (allocs n)

main :: IO ()
main = withSystemTempDirectory "compile" $ \tmp -> mainWith $ do

  -- nixpkgs: 1b27260
  -- ghc: 8.4.1

  f "()"                             144 renderByteString ()
  f "Int"                            216 renderByteString (123456789 :: Int)
  f "Word"                           216 renderByteString (123456789 :: Word)
  f "Char"                           232 renderByteString 'a'
  f "Integer"                        248 renderByteString (123456789 :: Integer)
  f "Proxy"                          280 renderByteString (Proxy :: Proxy "a")
  f "oneElement Proxy"               280 (renderByteString . S.oneElement) (Proxy :: Proxy "b")
  f "oneElement ()"                  280 (renderByteString . S.oneElement) ()
  f "oneAttribute ()"                280 (renderByteString . A.class_) ()
  f "oneAttribute Proxy"             280 (renderByteString . A.class_) (Proxy :: Proxy "c")
  f "listElement"                    352 (renderByteString . S.listElement) ()
  f "Double"                         360 renderByteString (123456789 :: Double)
  f "oneElement"                     368 (renderByteString . S.oneElement) ""
  f "nestedElement"                  368 (renderByteString . S.nestedElement) ""
  f "listOfAttributes"               352 (\x -> renderByteString [A.class_ x, A.class_ x]) ()
  f "Float"                          400 renderByteString (123456789 :: Float)
  f "oneAttribute"                   408 (renderByteString . A.class_) ""
  f "parallelElement"                520 (renderByteString . S.parallelElement) ""
  f "parallelAttribute"              584 (\x -> renderByteString $ A.class_ x # A.id_ x) ""
  f "elementWithAttribute"           584 (\x -> renderByteString $ div_A (A.class_ x) x) ""
  f "listOfListOf"                   632 (\x -> renderByteString $ div_ [i_ [span_ x]]) ()
  f "helloWorld"                    1232 (renderByteString . M.helloWorld) ()
  f "page"                          1512 (renderByteString . M.page) ()
  f "table"                         1664 (renderByteString . M.table) (2,2)
  f "AttrShort"                     2608 (renderByteString . M.attrShort) ()
  f "pageA"                         2960 (renderByteString . M.pageA) ()
  f "AttrLong"                      3352 (renderByteString . M.attrLong) ()
  f "Big table"                    19968 (renderByteString . M.table) (15,15)
  f "Big page"                     25064 (renderByteString . B.page) ()

  validateAction "Compile Library"   (compile tmp) "Html"                      $ allocsError 103 7
  validateAction "Compile Small.hs"  (compile tmp) "Small"                     $ allocsError 107 7
  validateAction "Compile Medium.hs" (compile tmp) "Medium"                    $ allocsError 180 7
  validateAction "Compile Big.hs"    (compile tmp) "Big"                       $ allocsError 248 7
  validateAction "Compile Alloc.hs"  (compile tmp) "bench/Alloc.hs"            $ allocsError 254 7
  validateAction "Compile Perf.hs"   (compile tmp) "bench/Perf.hs"             $ allocsError 482 7
  let x0 n = allocsError (107 + n) 7
  validateAction "Compile X0.hs"     (compile tmp) "bench/Compilation/X0.hs"   $ x0   0
  validateAction "Compile X1.hs"     (compile tmp) "bench/Compilation/X1.hs"   $ x0   1
  validateAction "Compile X2.hs"     (compile tmp) "bench/Compilation/X2.hs"   $ x0   3
  validateAction "Compile X4.hs"     (compile tmp) "bench/Compilation/X4.hs"   $ x0   6
  validateAction "Compile X8.hs"     (compile tmp) "bench/Compilation/X8.hs"   $ x0  13
  validateAction "Compile X16.hs"    (compile tmp) "bench/Compilation/X16.hs"  $ x0  30
  validateAction "Compile X32.hs"    (compile tmp) "bench/Compilation/X32.hs"  $ x0  80
  validateAction "Compile X64.hs"    (compile tmp) "bench/Compilation/X64.hs"  $ x0 237
  validateAction "Compile X128.hs"   (compile tmp) "bench/Compilation/X128.hs" $ x0 783

compile :: String -> String -> IO ()
compile out m =
  void . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags (dflags {optLevel = 2, importPaths = ["src", "bench"], hiDir = Just out, objectDir = Just out, outputFile = Just (out ++ "/out")})
    target <- guessTarget m Nothing
    setTargets [target]
    load LoadAllTargets
