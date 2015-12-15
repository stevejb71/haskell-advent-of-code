module Day9 where

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)
import Data.List (delete)

type City = String
type Distance = Int
type Route = ((City, City), Distance)
type Graph = [Route]

parse :: String -> Route
parse s = ((from, to), read dist) where [from, _, to, _, dist] = words s
-- shortestRoute :: Graph -> Distance
-- shortestRoute [] = 0
-- shortestRoute cs = do
--   c <- cs
--   let cs' = delete c cs
--
shortestRouteFrom :: Route -> Graph -> Distance
shortestRouteFrom r [x] = snd r
shortestRouteFrom start g = minimumDefault distances 0
  where g' = delete start g
        nexts = nextRoutes start g'
        distances = fmap (flip shortestRouteFrom g') nexts

minimumDefault :: Ord a => [a] -> a -> a
minimumDefault [] def = def
minimumDefault as _ = minimum as

nextRoutes :: Route -> Graph -> [Route]
nextRoutes ((start,end),_) = filter (\((c1,c2),_) -> (c1 `elem` [start,end] || c2 `elem` [start,end]))

distance :: Route -> Distance
distance = snd

graph = unsafePerformIO $ readFile "input.txt" >>= return . fmap parse . lines
