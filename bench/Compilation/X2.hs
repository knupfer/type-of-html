{-# LANGUAGE DataKinds #-}

module Compilation.X2 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 2) $ helloWorld ()

