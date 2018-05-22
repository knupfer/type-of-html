{-# LANGUAGE DataKinds #-}

module Compilation.X128 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 128) $ helloWorld ()

