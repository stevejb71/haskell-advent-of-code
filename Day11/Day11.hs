module Day11 where

import Data.Char (ord, chr)

input :: String
input = "cqjxjnds"

part1 :: String -> String
part1 = head . filter securePassword . iterate increment

part2 :: String -> String
part2 = part1 . increment . part1

securePassword :: String -> Bool
securePassword s = any isAscending (windows 3 s) && noConfusingChars s && (pairsCount s == 2)

pairsCount :: String -> Int
pairsCount [] = 0
pairsCount [_] = 0
pairsCount (x:y:zs) = if x == y then 1 + pairsCount zs else pairsCount (y:zs)

isAscending :: String -> Bool
isAscending [x,y,z] = ord x == ord y -1 && ord y == ord z - 1

noConfusingChars :: String -> Bool
noConfusingChars = not . any (`elem` "iol")

increment :: String -> String
increment = reverse . incrementR . reverse
  where incrementR ('z':cs) = 'a':(incrementR cs)
        incrementR (c:cs) = (incrementChar c):cs

incrementChar :: Char -> Char
incrementChar ch = chr (ord ch + 1)

windows :: Int -> [a] -> [[a]]
windows n xs = reverse $ go (length xs) n xs []
  where go _ _ [] acc = acc
        go len n l@(x:xs) acc =
          if len >= n then go (len-1) n xs (take n l : acc)
          else acc
