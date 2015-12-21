module Day9 where

import Data.List (delete)

type City = String
type Distance = Int
type Route = ((City, City), Distance)
type Graph = [Route]

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 = shortestDistance . fmap parse . take 13 . lines

parse :: String -> Route
parse s = ((from, to), read dist) where [from, _, to, _, dist] = words s

shortestDistance :: [Route] -> Int
shortestDistance = minimum . map distance . routes

distance :: [Route] -> Int
distance = sum . map snd

routes :: Graph -> [[Route]]
routes [] = undefined
routes g@(r:_) = routesFrom r g

routesFrom :: Route -> Graph -> [[Route]]
routesFrom _ [r] = [[r]]
routesFrom start g = map (start:) $ concatMap (`routesFrom` g') nextRs
  where nextRs = delete start (nextRoutes start g)
        g' = delete start g

nextRoutes :: Route -> Graph -> [Route]
nextRoutes ((start,end),_) = filter (\((c1,c2),_) -> (c1 `elem` [start,end] || c2 `elem` [start,end]))
