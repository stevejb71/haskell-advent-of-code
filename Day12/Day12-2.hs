{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}

module Day12 where

import Data.Aeson (decode, Value(..))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, toBoundedInteger)
import qualified Data.HashMap.Strict as H

main :: IO ()
main = B.readFile "input.txt" >>= print . countInts

countInts :: B.ByteString -> Int
countInts = count . json
  where json s = (fromJust . decode $ s) :: Value
        count value = case value of
          Number n -> toInt n
          Array as -> sum . fmap count $ as
          Bool _ -> 0
          String _ -> 0
          Null -> 0
          Object vs ->
            if "red" `elem` (H.elems vs)
              then 0
              else sum . fmap count $ H.elems vs

toInt :: Scientific -> Int
toInt = fromJust . toBoundedInteger

s :: B.ByteString
s = "[1,{\"c\":\"red\",\"b\":2},3]"
