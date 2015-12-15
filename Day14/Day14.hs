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

tuplize :: (a, [b]) -> [(a, b)]
tuplize (a, bs) = fmap (\b -> (a, b)) bs

accumScores :: [(Reindeer, Score)] -> [(Reindeer, Distance)] -> [(Reindeer, Score)]
accumScores scores distances = increment winner scores
  where winner = maxBySnd distances

increment :: Eq a => a -> [(a, Int)] -> [(a, Int)]
increment a = go []
  where go acc [] = acc
        go acc ((x, n):rest) = go ((if x == a then (x,n+1) else (x,n)):acc) rest

maxBySnd :: Ord b => [(a, b)] -> a
maxBySnd = fst . maximumBy (\x y -> snd x `compare` snd y)

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

rudolph = parse "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds."
cupid = parse "Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds."
