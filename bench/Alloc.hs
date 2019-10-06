{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE CPP            #-}

-- | Note that the allocation numbers are only reproducible on linux using the nix shell.

module Main where

import Html
import qualified Html.Attribute as A
import qualified Small          as S
import qualified Medium         as M
import qualified Big            as B

import Weigh
import Data.Proxy
import Data.Int

import GHC
import GHC.Paths (libdir)
import GHC.TypeNats
import DynFlags
import Control.Monad

allocs :: Int64 -> Weight -> Maybe String
allocs n w
  | n' > n = Just $ "More" ++ answer
  | n' < n = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = weightAllocatedBytes w
        answer = " allocated bytes than expected: " ++ show (abs $ n' - n)

allocsError :: (Show a, Integral a) => a -> a -> a -> Weight -> Maybe String
allocsError i m n w
  | n' > (m'+1) = Just $ "More" ++ answer
  | n' < (m'-1) = Just $ "Less" ++ answer
  | otherwise = Nothing
  where n' = round (fromIntegral (weightAllocatedBytes w) / (10^i) :: Rational)
        m' = n + m
        answer = " allocated bytes than expected: " ++ pretty (abs $ m' - n')
        pretty x = show x ++ " e" ++ show i

f :: Document b => String -> (a -> b) -> a -> Int64 -> Weigh ()
f s g x n = validateFunc s (renderByteString . g) x (allocs n)

main :: IO ()
main = mainWith $ do

  let ghc = allocFold                                    ::  GhcVersions  [802,  804,  806,   808]

  f "()"                   id ()                                   $ ghc   128     0     0      0
  f "Int"                  id (123456789 :: Int)                   $ ghc   192     0     0      0
  f "Word"                 id (123456789 :: Word)                  $ ghc   192     0     0      0
  f "Char"                 id 'a'                                  $ ghc   192     0     0      0
  f "Integer"              id (123456789 :: Integer)               $ ghc   248     0     0      0
  f "Proxy"                id (Proxy :: Proxy "a")                 $ ghc   208     0     0      0
  f "oneElement Proxy"     S.oneElement (Proxy :: Proxy "b")       $ ghc   208     0     0      0
  f "oneElement ()"        S.oneElement ()                         $ ghc   208     0     0      0
  f "oneAttribute ()"      A.class_ ()                             $ ghc   208     0     0      0
  f "oneAttribute Proxy"   A.class_ (Proxy :: Proxy "c")           $ ghc   208     0     0      0
  f "listElement"          S.listElement ()                        $ ghc   608     0     0      0
  f "Double"               id (123456789 :: Double)                $ ghc   360     0     0      0
  f "oneElement"           S.oneElement ""                         $ ghc   368     0     0      0
  f "nestedElement"        S.nestedElement ""                      $ ghc   368     0     0      0
  f "listOfAttributes"     (\x -> [A.class_ x, A.class_ x]) ()     $ ghc   712     0     0      0
  f "Float"                id (123456789 :: Float)                 $ ghc   400     0     0      0
  f "oneAttribute"         A.class_ ""                             $ ghc   520     0     0      0
  f "parallelElement"      S.parallelElement ""                    $ ghc   520     0     0   (-16)
  f "parallelAttribute"    (\x -> A.class_ x # A.id_ x) ""         $ ghc   736     0     0      0
  f "elementWithAttribute" (\x -> div_A (A.class_ x) x) ""         $ ghc   696     0     0      0
  f "listOfListOf"         (\x -> div_ [i_ [span_ x]]) ()          $ ghc  1200     0    64      0
  f "helloWorld"           M.helloWorld ()                         $ ghc   920   168     0      0
  f "page"                 M.page ()                               $ ghc  1400     0   720      0
  f "table"                M.table (2,2)                           $ ghc  2640     0     8   (-96)
  f "AttrShort"            M.attrShort ()                          $ ghc  2688     0  2104      0
  f "pageA"                M.pageA ()                              $ ghc  4552  (-96) (-96) (-288)
  f "AttrLong"             M.attrLong ()                           $ ghc  2688     0  2104      0
  f "Big table"            M.table (15,15)                         $ ghc 54040     0     8 (-3736)
  f "Big page"             B.page ()                               $ ghc 27888 (-120)    0 (-1344)

type family GhcVersions xs where
  GhcVersions '[] = Int64
  GhcVersions (x ': xs) = GHC x -> GhcVersions xs

newtype GHC (k :: Nat) = GHC Int64 deriving Num

class AllocFold a where
  allocFold :: a

instance AllocFold (GHC k -> Int64) where
  allocFold (GHC i) = i

instance AllocFold (GHC m -> a) => AllocFold (GHC l -> GHC m -> a) where
  allocFold (GHC i1) i2 = allocFold (GHC i1 + i2)

instance {-# OVERLAPPING #-} AllocFold (GHC __GLASGOW_HASKELL__ -> a) => AllocFold (GHC __GLASGOW_HASKELL__ -> GHC m -> a) where
  allocFold i1 _ = allocFold i1

compile :: String -> String -> IO ()
compile out m =
  void . defaultErrorHandler defaultFatalMessager defaultFlushOut . runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags (dflags {optLevel = 2, importPaths = ["src", "bench"], hiDir = Just out, objectDir = Just out, outputFile = Just (out ++ "/out")})
    target <- guessTarget m Nothing
    setTargets [target]
    load LoadAllTargets
