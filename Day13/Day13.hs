{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Attoparsec.Text (parseOnly, decimal, Parser, many1, letter)
import Data.List (nub, permutations)
import qualified Data.Map.Strict as M

type Guest = Text
type SeatingArrangement = [(Guest, Guest)]
type HappinessGains = M.Map (Guest, Guest) Int

main :: IO ()
main = readFile "input.txt" >>= print . part2

part1 :: String -> Int
part1 = bestScore . unsafeParseAll . map pack . lines

part2 :: String -> Int
part2 = bestScore . addMe . unsafeParseAll . map pack . lines

bestScore :: HappinessGains -> Int
bestScore hg = maximum $ map (score hg) (seatingArrangements . guests $ hg)

guests :: HappinessGains -> [Guest]
guests = nub . concatMap (untup . fst) . M.toList
  where untup (a,b) = [a,b]

score :: HappinessGains -> SeatingArrangement -> Int
score hg = sum . map (hg M.!)

happinessGain :: Parser ((Guest, Guest), Int)
happinessGain = tuplize <$> (guest <* " would ")
  <*> (("lose" <|> "gain") <* " ") <*> (decimal <* " happiness units by sitting next to ") <*> guest
  where tuplize g1 lg n g2 = ((g1, g2), n * (toSign lg))
        toSign "lose" = -1
        toSign "gain" = 1

unsafeParse :: Parser a -> Text -> a
unsafeParse p = fromRight . parseOnly p
  where fromRight (Right a) = a

unsafeParseAll :: [Text] -> HappinessGains
unsafeParseAll = M.fromList . map (unsafeParse happinessGain)

seatingArrangements :: [Guest] -> [SeatingArrangement]
seatingArrangements = map (concatMap double . seatingArrangement) . permutations
  where seatingArrangement xs = windows 2 (xs ++ [head xs])
        double [a,b] = [(a,b),(b,a)]

addMe :: HappinessGains -> HappinessGains
addMe hg = hg `M.union` meHG
  where gs = guests hg
        meHG = M.fromList $ concatMap (\g -> [((g, "Me"),0),(("Me", g),0)]) gs

guest :: Parser Guest
guest = pack <$> many1 letter

windows :: Int -> [a] -> [[a]]
windows n xs = reverse $ go (length xs) n xs []
  where go _ _ [] acc = acc
        go len n l@(x:xs) acc =
          if len >= n then go (len-1) n xs (take n l : acc)
          else acc
