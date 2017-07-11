{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Html
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T

main :: IO ()
main = T.putStrLn $ T.toLazyText $ render (bigTable (1000,50))

bigTable :: (Int, Int) -> 'Table > ['Tr > ['Td > String]]
bigTable (n, m) = table_ $ replicate n (tr_ $ map (td_ . show) [1..m])
