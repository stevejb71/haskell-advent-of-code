module Main where

data Track = Track {digit :: Char, count :: Int} deriving Show

main :: IO ()
main = print $ characterCount 40 1321131112

characterCount :: Int -> Int -> Int
characterCount repeats = length . last . take (repeats+1) . iterate lookAndSay . show

lookAndSay :: String -> String
lookAndSay = concat . reverse . doLookAndSay Nothing []

doLookAndSay :: Maybe Track -> [String] -> String -> [String]
doLookAndSay Nothing result "" = result
doLookAndSay Nothing result (c:cs) = doLookAndSay (Just $ Track c 1) result cs
doLookAndSay (Just (Track x n)) result "" = [x] : show n : result
doLookAndSay (Just (Track x n)) result (c:cs) =
  if x == c then doLookAndSay (Just (Track c (n+1))) result cs
  else doLookAndSay (Just (Track c 1)) ([x] : show n : result) cs
