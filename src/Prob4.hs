module Prob4
  ( main1
  , solve1
  , solve2
  ) where

import           ClassyPrelude
import           Data.List      (unfoldr)
import qualified Data.Text      as T
import qualified Data.Text.Read as T

main1 :: IO ()
main1 = readFileUtf8 "input4.in" >>= solve2

eitherFail :: Either a b -> IO b
eitherFail (Left _)  = fail "Bad input."
eitherFail (Right v) = return v

extract :: [(Int, Text)] -> Either String (Int, Int)
extract [(lb, _), (ub, _)] = Right (lb, ub)
extract _                  = Left "Bad input."

solve1 :: Text -> IO ()
solve1 txt = do
  (lb, ub) <-
    eitherFail . (>>= extract) . traverse T.decimal $ T.splitOn "-" txt
  print $ count isProper [lb .. ub]

genDigits :: Int -> [Int]
genDigits = reverse . unfoldr genDigits'
  where
    genDigits' n
      | n < 0 = Nothing
      | n < 10 = Just (n, -1)
      | otherwise = Just (n `mod` 10, n `div` 10)

isProper :: Int -> Bool
isProper number = uncurry (&&) . foldl' validate (True, False) $ pairs
  where
    pairs = zip digits (drop 1 digits)
    digits = genDigits number

validate :: (Bool, Bool) -> (Int, Int) -> (Bool, Bool)
validate (isValid, hadDoubles) (lhs, rhs)
  | lhs == rhs = (isValid, True)
  | lhs < rhs = (isValid, hadDoubles)
  | otherwise = (False, False)

count :: (a -> Bool) -> [a] -> Int
count f = foldl' counting 0
  where
    counting acc e =
      if f e
        then succ acc
        else acc

solve2 :: Text -> IO ()
solve2 txt = do
  (lb, ub) <-
    eitherFail . (>>= extract) . traverse T.decimal $ T.splitOn "-" txt
  print $ count isProper' [lb .. ub]

isProper' :: Int -> Bool
isProper' number = isAsc && hasPair'
  where
    hasPair' = hasPair digits
    isAsc = foldl' (&&) True $ uncurry (<=) <$> zip digits (drop 1 digits)
    hasPair [x, y] = x == y
    hasPair (x:y:z:rest) =
      if x == y
        then y /= z || hasPair (dropWhile (== y) rest)
        else hasPair (y : z : rest)
    hasPair _ = False
    digits = genDigits number
