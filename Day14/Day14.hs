module Day14 where

import qualified Data.Map as M
import Data.List (find, foldl', maximumBy)
import Data.Maybe (fromJust)

type Seconds = Int
type Distance = Int
type Score = Int
data Reindeer = Reindeer {name :: String, speed :: Int, flyTime :: Seconds, restPeriod :: Seconds} deriving (Eq, Show)

part1 :: IO ()
part1 = readFile "input.txt" >>= print . maximum . fmap (distanceTravelled 2503 . parse) . lines

part2 :: IO ()
part2 = readFile "input.txt" >>= print . maximum . fmap snd . scores 2503 . fmap parse . lines

scores :: Seconds -> [Reindeer] -> [(Reindeer, Score)]
scores secs rds = foldl' accumScores (initialScores rds) (take secs . distancesTravelled $ rds)

initialScores :: [Reindeer] -> [(Reindeer, Score)]
initialScores = fmap (\r -> (r,0))

accumScores :: [(Reindeer, Score)] -> [(Reindeer, Distance)] -> [(Reindeer, Score)]
accumScores scores = foldl' increment scores . maxBySnd

increment :: Eq a => [(a, Score)] -> a -> [(a, Score)]
increment scores a = go [] scores
  where go acc [] = acc
        go acc ((x, n):rest) = go ((if x == a then (x,n+1) else (x,n)):acc) rest

maxBySnd :: Ord b => [(a, b)] -> [a]
maxBySnd = fmap fst . maximaBy snd

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f [] = []
maximaBy f (a:as) = go [a] (f a) as
  where go acc _ [] = acc
        go acc eval (a:as) = case f a `compare` eval of
                                LT -> go acc eval as
                                EQ -> go (a:acc) eval as
                                GT -> go [a] (f a) as

distancesTravelled :: [Reindeer] -> [[(Reindeer, Distance)]]
distancesTravelled rs = fmap (oneTick rs) [1..]
  where oneTick rs sec = map (\r -> (r,distanceTravelled sec r)) rs

distanceTravelled :: Seconds -> Reindeer -> Distance
distanceTravelled = go 0
  where go acc time r@(Reindeer _ speed flyTime restPeriod)
          | time <= 0 = acc
          | otherwise = go (acc + speed * (min flyTime time)) (time - flyTime - restPeriod) r

parse :: String -> Reindeer
parse s = Reindeer name (read speed) (read flyTime) (read restPeriod)
  where [name, speed, flyTime, restPeriod] = readWords (words s) [0, 3, 6, 13]
        readWords ws = fmap (ws !!)
