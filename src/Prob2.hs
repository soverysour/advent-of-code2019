module Prob2
  ( main1
  , solve1
  ) where

import           ClassyPrelude
import           Control.Monad.Trans.State.Lazy
import qualified Data.Text                      as T
import qualified Data.Text.Read                 as T
import qualified Data.Vector                    as V

main1 :: IO ()
main1 = readFileUtf8 "input2.in" >>= solve1

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
      out = vec V.!? (pos + 3) >>= (vec V.!?)
      out' = fromMaybe 0 out
  modify' (V.// [(out', val)])
  iterating $ pos + 4
