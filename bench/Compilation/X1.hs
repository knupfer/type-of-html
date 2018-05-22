{-# LANGUAGE DataKinds #-}

module Compilation.X1 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 1) $ helloWorld ()

