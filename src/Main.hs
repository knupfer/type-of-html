{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Html

main :: IO ()
main
  = print
  . div_
  $ div_ "TEST"

