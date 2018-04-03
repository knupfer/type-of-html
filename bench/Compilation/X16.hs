{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE DataKinds #-}

module Compilation.X16 (run) where

import Compilation

run :: IO ()
run = print . r (Proxy :: Proxy 16) $ helloWorld ()

