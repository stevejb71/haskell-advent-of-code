module Main where

import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)

main :: IO ()
main = print part2

part1 :: Int
part1 = findHashNumber startsWith5Zeroes "bgvyzdsv"

part2 :: Int
part2 = findHashNumber startsWith6Zeroes "bgvyzdsv"

findHashNumber :: (String -> Bool) -> String -> Int
findHashNumber f s = head $ filter (\n -> f $ md5s (s ++ show n)) [1..]

md5s :: String -> String
md5s = show . md5 . pack

startsWith5Zeroes :: String -> Bool
startsWith5Zeroes = (== "00000") . take 5

startsWith6Zeroes :: String -> Bool
startsWith6Zeroes = (== "000000") . take 6
