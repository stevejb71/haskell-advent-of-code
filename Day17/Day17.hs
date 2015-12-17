module Day17 where

import Data.List.Split

type Size = Int
type Containers = [Size]

part1 :: IO ()
part1 = readFile "input.txt" >>= print . solveInput1 150

part2 :: IO ()
part2 = readFile "input.txt" >>= print . solveInput2 150

solveInput1 :: Size -> String -> Int
solveInput1 s input = length $ containers s cs
  where cs = fmap read $ lines input

solveInput2 :: Size -> String -> Int
solveInput2 s input = length minConts
  where cs = fmap read $ lines input
        conts = containers s cs
        minNum = minimum $ map length conts
        minConts = filter (\cs -> length cs == minNum) conts

containers :: Size -> Containers -> [Containers]
containers 0 cs = [[]]
containers _ [] = []
containers n [x] = if x == n then [[n]] else []
containers n (c:cs) = containers n cs ++ (map (c:) $ containers (n-c) cs)
