module Day5 where

main :: IO ()
main = readFile "input.txt" >>= print . length . filter isNice . lines

isNice :: String -> Bool
isNice s = and $ fmap ($ s) [not . hasDisallowedStrings, has2InARow, hasAtLeast3Vowels]

hasDisallowedStrings :: String -> Bool
hasDisallowedStrings = any (`elem` ["ab", "cd", "pq", "xy"]) . windows 2

has2InARow :: String -> Bool
has2InARow = any (\s -> s !! 0 == s !! 1) . windows 2

hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels s = (length . filter (`elem` "aeiou") $ s) >= 3

windows :: Int -> [a] -> [[a]]
windows n xs = reverse $ go (length xs) n xs []
  where go _ _ [] acc = acc
        go len n l@(x:xs) acc =
          if len >= n then go (len-1) n xs (take n l : acc)
          else acc
