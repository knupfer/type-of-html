{-# LANGUAGE DataKinds #-}

module Compilation.X4 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 4) $ helloWorld ()

