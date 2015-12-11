module Main where

data Track = Track {digit :: Char, count :: Int} deriving Show

main :: IO ()
main = repeat50

repeat50 :: IO ()
repeat50 = print $ characterCount 50 1321131112

repeat40 :: IO ()
repeat40 = print $ characterCount 40 1321131112

characterCount :: Int -> Int -> Int
characterCount repeats = length . last . take (repeats+1) . iterate lookAndSay . show

lookAndSay :: String -> String
lookAndSay = drop 2 . concat . reverse . doLookAndSay (Track '0' 0) []
  where doLookAndSay (Track x n) result "" = [x] : show n : result
        doLookAndSay (Track x n) result (c:cs) =
          if x == c then doLookAndSay (Track c (n+1)) result cs
          else doLookAndSay (Track c 1) ([x] : show n : result) cs
