module Main where

main :: IO ()
main = print $ codes !! indexOf 2947 3029

codes :: [Int]
codes = iterate next 20151125

next :: Int -> Int
next n = (n * 252533) `rem` 33554393

indexOf :: Int -> Int -> Int
indexOf r c = (r * (r-1) + (c-1) * (c+2*r)) `div` 2
