{-# LANGUAGE TupleSections #-}

module Prob3
  ( main1
  , solve1
  , solve2
  ) where

-- TODO: Use a Map ( O: )
import           ClassyPrelude
import qualified Data.Set       as S
import qualified Data.Text      as T
import qualified Data.Text.Read as T

type Pos = (Int, Int)

type StateV = (Pos, Int, S.Set (Int, Pos))

data Direction
  = DLeft
  | DRight
  | DUp
  | DDown
  deriving (Eq, Ord, Enum, Bounded, Show)

type Order = (Direction, Int)

main1 :: IO ()
main1 = readFileUtf8 "input3.in" >>= solve2

mkOrder :: Text -> Maybe Order
mkOrder txt =
  let prefix = T.take 1 txt
      number =
        case T.decimal . T.drop 1 $ txt of
          Left _       -> Nothing
          Right (v, _) -> Just v
   in case prefix of
        "R" -> (DRight, ) <$> number
        "L" -> (DLeft, ) <$> number
        "U" -> (DUp, ) <$> number
        "D" -> (DDown, ) <$> number
        _   -> Nothing

extract :: Text -> Maybe [Order]
extract = traverse mkOrder . T.splitOn ","

solve1 :: Text -> IO ()
solve1 txt = do
  let maybeOrders = traverse extract . filter (/= "") $ T.lines txt
  case maybeOrders of
    Just [orders1, orders2] -> do
      let (_, _, firstSet) = perform orders1
          (_, _, secondSet) = perform orders2
      print . closestIntersection $
        S.map snd firstSet `S.intersection` S.map snd secondSet
    _ -> print ("Bad input." :: Text)

solve2 :: Text -> IO ()
solve2 txt = do
  let maybeOrders = traverse extract . filter (/= "") $ T.lines txt
  case maybeOrders of
    Just [orders1, orders2] -> do
      let (_, _, firstSet) = perform orders1
          (_, _, secondSet) = perform orders2
      print $
        getMin . catMaybes $ S.toList $ S.map (pairwise firstSet) secondSet
    _ -> print ("Bad input." :: Text)

initialState :: StateV
initialState = ((0, 0), 0, S.empty)

perform :: [Order] -> StateV
perform [] = initialState
perform (x:xs) = flip (foldl' combine) xs . extract00 $ combine initialState x
  where
    extract00 (pos, act, set) =
      (pos, act, S.filter (\(_, v) -> v /= (0, 0)) set)

combine :: StateV -> Order -> StateV
combine ((x, y), act, set) (DRight, move) =
  ( (x + move, y)
  , act + move
  , S.fromList (zip [act .. act + move] ((, y) <$> [x .. x + move])) `S.union`
    set)
combine ((x, y), act, set) (DUp, move) =
  ( (x, y + move)
  , act + move
  , S.fromList (zip [act .. act + move] ((x, ) <$> [y .. y + move])) `S.union`
    set)
combine ((x, y), act, set) (DLeft, move) =
  ( (x - move, y)
  , act + move
  , S.fromList (zip [act .. act + move] ((, y) <$> [x,x - 1 .. x - move])) `S.union`
    set)
combine ((x, y), act, set) (DDown, move) =
  ( (x, y - move)
  , act + move
  , S.fromList (zip [act .. act + move] ((x, ) <$> [y,y - 1 .. y - move])) `S.union`
    set)

closestIntersection :: Set Pos -> Maybe Int
closestIntersection = S.foldl' f Nothing
  where
    manhattan x y = abs x + abs y
    f Nothing (x, y) = Just $ manhattan x y
    f (Just v) (x, y) =
      let manhattan' = manhattan x y
       in if manhattan' < v
            then Just manhattan'
            else Just v

pairwise :: Set (Int, Pos) -> (Int, Pos) -> Maybe Int
pairwise set (cost, pos) =
  case S.toList $ S.filter (\(_, pos') -> pos == pos') set of
    [(cost', _)] -> Just $ cost + cost'
    _            -> Nothing

getMin :: [Int] -> Maybe Int
getMin [] = Nothing
getMin (x:xs) =
  case getMin xs of
    Nothing -> Just x
    Just x' ->
      if x < x'
        then Just x
        else Just x'
