{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly)
import Prelude hiding (lines)
import Data.Text (Text, pack, lines)
import qualified Data.Vector.Unboxed as G
import qualified Data.Vector.Unboxed.Mutable as M

type Action = Bool -> Bool
type Light = (Int, Int)
data Instruction = Instruction {action :: Action, start :: Light, end :: Light}
type Display = M.IOVector Bool

main :: IO ()
main = part1 >>= print

countLights :: G.Vector Bool -> Int
countLights = G.length . G.filter id

part1 :: IO Int
part1 = do
  s <- readFile "input.txt"
  let is = map unsafeParse . lines . pack $ s
  d <- initialDisplay
  _ <- execute d is
  d' <- G.freeze d
  return $ countLights d'

initialDisplay :: IO Display
initialDisplay = M.replicate 1000000 False

execute :: Display -> [Instruction] -> IO ()
execute d = mapM_ executeI
  where executeI (Instruction f (x1,y1) (x2,y2)) = sequence_ [M.modify d f (x+y*1000) | x <- [x1..x2], y <- [y1..y2]]

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
