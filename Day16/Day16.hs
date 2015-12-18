{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)

type SueId = Int
type Compound = Text
type Traces = [(Compound, Int)]

data Sue = Sue {number :: SueId, traces :: Traces} deriving (Show)

main :: IO ()
main = readFile "input.txt" >>= print . run foundTraces2

run :: M.Map Compound (Int -> Bool) -> String -> [Sue]
run tracesMap s = filter (sueMatchesFoundTraces tracesMap) . fmap parseUnsafe . lines $ s

parseUnsafe :: String -> Sue
parseUnsafe s = parsedSue
  where (Right parsedSue) = parseOnly sue (pack s)

foundTraces :: M.Map Compound (Int -> Bool)
foundTraces = M.fromList $ map (\(x,y) -> (x,(== y))) [("children", 3), ("cats", 7), ("samoyeds", 2),
  ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5),
  ("trees", 3), ("cars", 2), ("perfumes", 1)
  ]

foundTraces2 :: M.Map Compound (Int -> Bool)
foundTraces2 = M.fromList [("children", (== 3)), ("cats", (> 7)), ("samoyeds", (== 2)),
  ("pomeranians", (< 3)), ("akitas", (== 0)), ("vizslas", (== 0)), ("goldfish", (< 5)),
  ("trees", (> 3)), ("cars", (== 2)), ("perfumes", (== 1))
  ]

sueMatchesFoundTraces :: M.Map Compound (Int -> Bool) -> Sue -> Bool
sueMatchesFoundTraces tracesMap = all matchesFound . traces
  where matchesFound (c, i) = (fromMaybe (const True) $ c `M.lookup` tracesMap) i

sue :: Parser Sue
sue = Sue <$> ("Sue " *> decimal) <*> (": " *> tracesP)

tracesP :: Parser Traces
tracesP = traceP `sepBy` (string ", ")
  where traceP = (,) <$> fmap pack (many1 letter <* ": ") <*> decimal
