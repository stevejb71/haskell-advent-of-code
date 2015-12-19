module Day12 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, signed, decimal, many1, sepBy, satisfy, parseOnly)
import Data.Text (pack, Text)
import Data.Char (isNumber)

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 :: String -> Int
part1 = sum . unsafeParse numbers . pack

numbers :: Parser [Int]
numbers = notNumbers *> (signed decimal `sepBy` notNumbers)
  where notNumbers = many1 . satisfy $ not . (\x -> isNumber x || (x == '-'))

unsafeParse :: Parser a -> Text -> a
unsafeParse p = fromRight . parseOnly p
  where fromRight (Right a) = a
