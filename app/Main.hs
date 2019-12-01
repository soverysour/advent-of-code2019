module Main
  ( main
  ) where

import           ClassyPrelude

import           Prob1

main :: IO ()
main = readFileUtf8 "input.in" >>= solve2
