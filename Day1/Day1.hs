module Main where

import Data.List (foldl')

main :: IO ()
main = readFile "input.txt" >>= print . calcFloor

calcFloor :: String -> Int
calcFloor s = foldl' step 0 s
  where step c '(' = c + 1
        step c ')' = c - 1
