{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly)
import Data.Text (Text, pack)
import Data.Matrix (matrix, Matrix, mapRow, toList)
import Data.List (foldl')

type Action = Bool -> Bool
type Light = (Int, Int)
data Instruction = Instruction {action :: Action, start :: Light, end :: Light}
type Display = Matrix Bool

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 :: String -> Int
part1 = countOn . executeAll . map (unsafeParse . pack) . lines

initialDisplay :: Display
initialDisplay = matrix 1000 1000 (const False)

countOn :: Display -> Int
countOn = length . filter id . toList

executeAll :: [Instruction] -> Display
executeAll is = foldl' execute initialDisplay (take 2 is)

execute :: Display -> Instruction -> Display
execute d (Instruction a (x1,y1) (x2,y2)) = foldl' (flip (mapRow f)) d [0]
  where f col = if col >= x1 && col <= x2 then a else id

unsafeParse :: Text -> Instruction
unsafeParse = fromRight . parseOnly instruction
  where fromRight (Right a) = a
        fromRight _ = error "failed to parse"

instruction :: Parser Instruction
instruction = Instruction <$> actionP <*> (" " *> light <* " through ") <*> light

light :: Parser Light
light = (,) <$> (decimal <* char ',') <*> decimal

actionP :: Parser Action
actionP = match "turn on" (const True) <|> match "turn off" (const False) <|> match "toggle" not
  where match s a = s >> pure a
