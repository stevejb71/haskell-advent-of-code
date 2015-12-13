module Day5 where

import Data.List (groupBy, sort)
import Data.List.Split (chunksOf)

part1 :: IO ()
part1 = readFile "input.txt" >>= print . length . filter isNice1 . lines

part2 :: IO ()
part2 = readFile "input.txt" >>= print . length . filter isNice2 . lines

isNice1 :: String -> Bool
isNice1 s = and $ fmap ($ s) [not . hasDisallowedStrings, has2InARow, hasAtLeast3Vowels]

isNice2 :: String -> Bool
isNice2 s = and $ fmap ($ s) [has2EqualPairs, hasSandwich]

hasDisallowedStrings :: String -> Bool
hasDisallowedStrings = any (`elem` ["ab", "cd", "pq", "xy"]) . windows 2

hasSandwich :: String -> Bool
hasSandwich = any isSandwich . windows 3
  where isSandwich [x,_,z] = x == z

has2InARow :: String -> Bool
has2InARow = any (\s -> s !! 0 == s !! 1) . windows 2

has2EqualPairs :: String -> Bool
has2EqualPairs [] = False
has2EqualPairs [_] = False
has2EqualPairs (a:b:xs) = ([a,b]) `elem` windows 2 xs || has2EqualPairs (b:xs)

hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels s = (length . filter (`elem` "aeiou") $ s) >= 3

windows :: Int -> [a] -> [[a]]
windows n xs = reverse $ go (length xs) n xs []
  where go _ _ [] acc = acc
        go len n l@(x:xs) acc =
          if len >= n then go (len-1) n xs (take n l : acc)
          else acc
