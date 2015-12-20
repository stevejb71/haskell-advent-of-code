module Main where

import Data.List (nub, sort)

type HouseNumber = Int
type ElfNumber = Int

main :: IO ()
main = print part1

part1 :: HouseNumber
part1 = fst . head . filter ((>= 33100000) . snd) $ zip [1..] presentsCounts

part2 :: HouseNumber
part2 = fst . head . filter ((>= 33100000) . snd) $ zip [1..] presentsCounts2

presentsCounts2 :: [Int]
presentsCounts2 = map ((11 *) . sum . elvesVisiting2) [1..]

presentsCounts :: [Int]
presentsCounts = map ((10 *) . sum . elvesVisiting) [1..]

elvesVisiting2 :: HouseNumber -> [ElfNumber]
elvesVisiting2 h = dropWhile (\e -> h >= 50 * e) . sort . nub $ (++) <$> id <*> map (h `div`) $ divisors
  where divisors = filter (\e -> h `rem` e == 0) [1..isqrt h]

elvesVisiting :: HouseNumber -> [ElfNumber]
elvesVisiting h = nub $ (++) <$> id <*> map (h `div`) $ divisors
  where divisors = filter (\e -> h `rem` e == 0) [1..isqrt h]

isqrt :: Int -> Int
isqrt = ceiling . sqrt . fromIntegral
