module Day14 where

import Data.List (foldl')

data Reindeer = Reindeer {name :: String, speed :: Int, flyTime :: Int, restPeriod :: Int} deriving Show

main :: IO ()
main = readFile "input.txt" >>= print . maximum . fmap (distanceTravelled 2503 . parse) . lines

distanceTravelled :: Int -> Reindeer -> Int
distanceTravelled = go 0
  where go acc time r@(Reindeer _ speed flyTime restPeriod)
          | time <= 0 = acc
          | otherwise = go (acc + speed * (min flyTime time)) (time - flyTime - restPeriod) r

parse :: String -> Reindeer
parse s = Reindeer name (read speed) (read flyTime) (read restPeriod)
  where [name, speed, flyTime, restPeriod] = readWords (words s) [0, 3, 6, 13]
        readWords ws = fmap (ws !!)
