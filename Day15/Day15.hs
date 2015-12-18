{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Data.Attoparsec.Text
import Data.Text (pack, Text)

type Quantity = Int
type Quantities = [Quantity]
type Score = Int
data Ingredient = Ingredient {name :: Text, capacity :: Int, durability :: Int, flavour :: Int, texture :: Int, calories :: Int} deriving Show

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 :: String -> Score
part1 s = bestScore is
  where is = map (unsafeParse . pack) . lines $ s

part2 :: String -> Score
part2 s = bestScore2 is
  where is = map (unsafeParse . pack) . lines $ s

unsafeParse :: Text -> Ingredient
unsafeParse = fromRight . parseOnly ingredient
  where fromRight (Right a) = a

bestScore :: [Ingredient] -> Score
bestScore = maximum . map fst . scoreIngredients

bestScore2 :: [Ingredient] -> Score
bestScore2 = maximum . map fst . filter ((== 500) . snd) . scoreIngredients

scoreIngredients :: [Ingredient] -> [(Score, Int)]
scoreIngredients is = zip <$> map (score is) <*> map (calorieCount is) $ qs
  where qs = quantities (length is) 100

calorieCount :: [Ingredient] -> [Quantity] -> Int
calorieCount is qs = sum $ map (\(a,b) -> a*b) $ (map calories is) `zip` qs

score :: [Ingredient] -> [Quantity] -> Score
score is qs = product [score1 capacity, score1 durability, score1 texture, score1 flavour]
  where score1 f = max 0 $ sum $ map (\(a,b) -> a*b) $ (map f is) `zip` qs

quantities :: Int -> Int -> [Quantities]
quantities 1 n = [[n]]
quantities num percent =
  if percent < 0
    then []
    else concatMap (\p -> map (p:) $ quantities (num-1) (percent-p)) [0..percent]

ingredient :: Parser Ingredient
ingredient = fmap makeIngredient ingredientDescription
  where makeIngredient (n, [cap, d, f, t, cal]) = Ingredient n cap d f t cal

ingredientDescription :: Parser (Text, [Int])
ingredientDescription = (,) <$> (textP <* ": ") <*> (textP <* " " *> signed decimal) `sepBy` ", "

textP :: Parser Text
textP = fmap pack $ many1 letter

butterscotch = unsafeParse "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
cinnamon = unsafeParse "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
is = [butterscotch, cinnamon]
