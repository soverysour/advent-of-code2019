module Prob2
  ( main1
  , solve1
  , solve2
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import qualified Data.Text                      as T
import qualified Data.Text.Read                 as T
import qualified Data.Vector                    as V

main1 :: IO ()
main1 = readFileUtf8 "input2.in" >>= solve2

upperBound :: Int
upperBound = 69

possibleVals :: [(Int, Int)]
possibleVals =
  let vals = [0 .. upperBound]
   in vals >>= flip fmap vals . (,)

solve2 :: Text -> IO ()
solve2 txt = do
  let maybeInts = traverse T.decimal . filter (/= "") $ T.splitOn "," txt
  case maybeInts of
    Left _ -> print ("Bad input." :: Text)
    Right vec ->
      let vec' = V.fromList $ fst <$> vec
          tryIt (one, two) acc =
            if evalState (iterating 0) (vec' V.// [(1, one), (2, two)]) ==
               Just 19690720
              then Just (one, two)
              else acc
          result = foldr tryIt Nothing possibleVals
       in case result of
            Nothing -> print ("No solution found." :: Text)
            Just (noun, verb) -> do
              print noun
              print verb
              print $ 100 * noun + verb

solve1 :: Text -> IO ()
solve1 txt = do
  let maybeInts = traverse T.decimal . filter (/= "") $ T.splitOn "," txt
  case maybeInts of
    Left _ -> print ("Bad input." :: Text)
    Right vec ->
      let vec' = V.fromList $ fst <$> vec
       in print $ evalState (iterating 0) (vec' V.// [(1, 12), (2, 2)])

iterating :: Int -> State (Vector Int) (Maybe Int)
iterating pos = do
  vec <- get
  case vec V.!? pos of
    Just 1  -> updated (+) pos
    Just 2  -> updated (*) pos
    Just 99 -> return $ vec V.!? 0
    _       -> return Nothing

updated :: (Int -> Int -> Int) -> Int -> State (Vector Int) (Maybe Int)
updated f pos = do
  vec <- get
  let lhs = vec V.!? (pos + 1) >>= (vec V.!?)
      rhs = vec V.!? (pos + 2) >>= (vec V.!?)
      val = fromMaybe 0 $ liftA2 f lhs rhs
      out = vec V.!? (pos + 3)
      out' = fromMaybe 0 out
  modify' (V.// [(out', val)])
  iterating $ pos + 4
