module Day2 where

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = readFile "input.txt" >>= print . totalArea

totalArea :: String -> Int
totalArea s = sum $ fmap (area . sides) (lines s)

area :: [Int] -> Int
area xs = a * b + 2 * (a * b) + 2 * (b * c) + 2 * (a * c) where [a, b, c] = xs

sides :: String -> [Int]
sides s = sort $ fmap read $ splitOn "x" s
