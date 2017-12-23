module Main where

import Data.Int (Int64)

series :: Int64 -> Int64 -> [Int64]
series initial factor = iterate (\x -> x * factor `mod` 2147483647) initial

main :: IO ()
main = print $ length $ filter id $ zipWith (==) as bs
  -- where as = scrutinize $ series 703 16807
  --       bs = scrutinize $ series 516 48271
  --       scrutinize = map (`mod` 65536) . take 40000000
  where as = scrutinize $ filter (\x -> x `mod` 4 == 0) $ series 703 16807
        bs = scrutinize $ filter (\x -> x `mod` 8 == 0) $ series 516 48271
        scrutinize = map (`mod` 65536) . take 5000000
