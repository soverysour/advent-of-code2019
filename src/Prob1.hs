module Prob1
  ( main1
  , solve1
  , solve2
  ) where

import           ClassyPrelude
import qualified Data.Text      as T
import qualified Data.Text.Read as T

main1 :: IO ()
main1 = readFileUtf8 "input1.in" >>= solve2

solve1 :: Text -> IO ()
solve1 txt = do
  let maybeInts = traverse T.decimal . filter (/= "") $ T.lines txt
  res <- extract $ proc1 . undecorate <$> maybeInts
  print res

solve2 :: Text -> IO ()
solve2 txt = do
  let maybeInts = traverse T.decimal . filter (/= "") $ T.lines txt
  res <- extract $ proc2 . undecorate <$> maybeInts
  print res

proc1 :: [Int] -> Int
proc1 = foldl' (+) 0 . fmap (subtract 2 . (`div` 3))

proc2 :: [Int] -> Int
proc2 = foldl' (+) 0 . fmap fuelRec

fuelRec :: Int -> Int
fuelRec v =
  let newF = subtract 2 (v `div` 3)
   in if newF <= 0
        then 0
        else newF + fuelRec newF

undecorate :: [(Int, Text)] -> [Int]
undecorate res = fst <$> res

extract :: Either String a -> IO a
extract (Left _)  = fail "No such value."
extract (Right a) = return a
