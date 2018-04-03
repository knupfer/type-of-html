{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE DataKinds #-}

module Compilation.X0 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 0) $ helloWorld ()

