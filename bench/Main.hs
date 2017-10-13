module Main where

import Blaze
import Small
import Medium
import Big

import Criterion.Main

main :: IO ()
main = defaultMain
  [ small
  , medium
  , big
  , blaze
  ]
