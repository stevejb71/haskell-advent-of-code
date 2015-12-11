module Main where

import Data.List (foldl')
import qualified Data.Map.Strict as M

type Move = Char
type Position = (Int, Int)
type State = (Position, M.Map Position Int)

main :: IO ()
main = readFile "input.txt" >>= print . M.size . countMoves

countMoves :: [Move] -> M.Map Position Int
countMoves = snd . foldl' recordMove ((0, 0), M.singleton (0,0) 1)

recordMove :: State -> Move -> State
recordMove  (p, cnt) m  = (p', M.insertWith (+) p' 1 cnt)
  where p' = move p m

move :: Position -> Move -> Position
move (x, y) '^' = (x, y - 1)
move (x, y) 'v' = (x, y + 1)
move (x, y) '<' = (x - 1, y)
move (x, y) '>' = (x + 1, y)
