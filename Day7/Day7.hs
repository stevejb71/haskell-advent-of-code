
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Applicative
import Data.Bits
import Data.Char (isLower)
import Data.Attoparsec.Text (Parser, string, satisfy, decimal, parseOnly, many1)
import Data.Text (Text, pack)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Debug.Trace

type Id = Text
data Wire = Wire Id Signal deriving (Show)
data Signal = Value Int | And Signal Signal
                | Or Signal Signal | Not Signal
                | LShift Signal Int | RShift Signal Int
                | WireInput Id deriving (Show)

main :: IO ()
main = readFile "input.txt" >>= print . part1

part1 :: String -> Int
part1 s = evalWire signals wireA
  where signals = parseAll s
        wireA = Wire "a" (signals M.! "a")

part2 :: String -> Int
part2 s = evalWire (override signals) wireA
  where signals = parseAll s
        wireA = Wire "a" (signals M.! "a")
        override = M.insert "b" (Value 46065)

parseAll :: String -> M.Map Id Signal
parseAll s = M.fromList (map tuple $ wires s)
  where wires = map (parse . pack) . lines
        tuple (Wire i s) = (i, s)

parse :: Text -> Wire
parse = fromRight . parseOnly wire
  where fromRight (Right a) = a

evalWire :: M.Map Id Signal -> Wire -> Int
evalWire signals (Wire i s) = evalState (evalSignal signals s) M.empty

evalSignal :: M.Map Id Signal -> Signal -> State (M.Map Id Int) Int
evalSignal signals s = do
  case s of
    Value n -> return n
    And s1 s2 -> (.&.) <$> eval s1 <*> eval s2
    Or s1 s2 -> (.|.) <$> eval s1 <*> eval s2
    Not s -> eval s >>= return . complement
    LShift s n -> eval s >>= return . (flip shiftL n)
    RShift s n -> eval s >>= return . (flip shiftR n)
    WireInput i -> do
      values <- get
      case i `M.lookup` values of
        Just n -> return n
        Nothing -> do
          v <- eval (signals M.! i)
          values' <- get
          put $ M.insert i v values'
          eval s
  where eval = evalSignal signals

wire :: Parser Wire
wire = (flip Wire) <$> signal <* string " -> " <*> wireId

wireId :: Parser Id
wireId = pack <$> many1 (satisfy isLower)

signal :: Parser Signal
signal = andP <|> orP <|> notP <|> lshiftP <|> rshiftP <|> value <|> wireInput

wireInput :: Parser Signal
wireInput = WireInput <$> wireId

notP :: Parser Signal
notP = Not <$> (string "NOT " *> signal)

lshiftP :: Parser Signal
lshiftP = LShift <$> gateInput <* string " LSHIFT " <*> decimal

rshiftP :: Parser Signal
rshiftP = RShift <$> gateInput <* string " RSHIFT " <*> decimal

andP :: Parser Signal
andP = And <$> gateInput <* string " AND " <*> gateInput

orP :: Parser Signal
orP = Or <$> gateInput <* string " OR " <*> gateInput

value :: Parser Signal
value = Value <$> decimal

gateInput :: Parser Signal
gateInput = wireInput <|> value
