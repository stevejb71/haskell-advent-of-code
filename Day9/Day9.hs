module Day9 where

import Data.List (delete)
import qualified Data.Vector as V

type City = String
type Distance = Int
type Route = ((City, City), Distance)
type Graph = V.Vector Route

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 = shortestDistance . fmap parse . V.take 13 . V.fromList . lines

parse :: String -> Route
parse s = ((from, to), read dist) where [from, _, to, _, dist] = words s

shortestDistance :: Graph -> Int
shortestDistance = minimum . map distance . routes

distance :: Graph -> Int
distance = sum . fmap snd

routes :: Graph -> [Graph]
routes g = routesFrom (V.head g) g

routesFrom :: Route -> Graph -> [Graph]
routesFrom start g =
  if V.length g == 1
    then [V.fromList [V.head g]]
    else fmap (start:) $ V.concatMap (`routesFrom` g') nextRs
  where nextRs = V.delete start (nextRoutes start g)
        g' = V.delete start g

nextRoutes :: Route -> Graph -> Graph
nextRoutes ((start,end),_) = V.filter (\((c1,c2),_) -> (c1 `elem` [start,end] || c2 `elem` [start,end]))
