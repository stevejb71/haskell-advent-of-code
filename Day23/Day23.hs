{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, signed, Parser, decimal, parseOnly)
import Prelude hiding (lines)
import Data.Text (Text, pack, lines)
import qualified Data.Vector as V

data Register = A | B deriving Show
type Offset = Int
data Instruction = Hlf Register | Tpl Register |
  Inc Register | Jmp Offset | Jie Register Offset |
  Jio Register Int deriving Show
data Cpu = Cpu {a :: Integer, b :: Integer, pc :: Int} deriving Show

main :: IO ()
main = readFile "input.txt" >>= print . part2

part1 :: String -> Cpu
part1 s = run (parseInstructions s) cpu0

part2 :: String -> Cpu
part2 s = run (parseInstructions s) cpu0 {a = 1}

cpu0 :: Cpu
cpu0 = Cpu 0 0 0

run :: [Instruction] -> Cpu -> Cpu
run is = go (fmap compile . V.fromList $ is)
  where end = length is
        go fs cpu =
          if pc cpu >= end
            then cpu
            else let f = readAtPC fs cpu in go fs (f cpu)

readAtPC :: V.Vector a -> Cpu -> a
readAtPC xs = (V.!) xs . pc

compile :: Instruction -> Cpu -> Cpu
compile i = case i of
  Hlf r -> modifyPC 1 . modifyRegister r (`div` 2)
  Tpl r -> modifyPC 1 . modifyRegister r (* 3)
  Inc r -> modifyPC 1 . modifyRegister r (+ 1)
  Jmp offset -> modifyPC offset
  Jie r offset -> \cpu -> modifyPC (if even (readRegister r cpu) then offset else 1) cpu
  Jio r offset -> \cpu -> modifyPC (if readRegister r cpu == 1 then offset else 1) cpu

modifyPC :: Offset -> Cpu -> Cpu
modifyPC o cpu = cpu {pc = o + pc cpu}

readRegister :: Register -> Cpu -> Integer
readRegister r = case r of
  A -> a
  B -> b

modifyRegister :: Register -> (Integer -> Integer) -> Cpu -> Cpu
modifyRegister r f = case r of
  A -> \c -> c {a = f (a c)}
  B -> \c -> c {b = f (b c)}

parseInstructions :: String -> [Instruction]
parseInstructions = map unsafeParse . lines . pack

unsafeParse :: Text -> Instruction
unsafeParse = fromRight . parseOnly instruction
  where fromRight (Right x) = x
        fromRight x = error ("failed: " ++ show x)

instruction :: Parser Instruction
instruction = Hlf <$> ("hlf " *> register) <|>
         Tpl <$> ("tpl " *> register) <|>
         Inc <$> ("inc " *> register) <|>
         Jmp <$> ("jmp " *> signed decimal) <|>
         Jie <$> ("jie " *> register <* ", ") <*> signed decimal <|>
         Jio <$> ("jio " *> register <* ", ") <*> signed decimal

register :: Parser Register
register = const A <$> char 'a' <|> const B <$> char 'b'
