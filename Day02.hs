module Main where

checksum :: [[Int]] -> Int
-- checksum sheet = sum $ map difference sheet
--   where difference xs = foldl1 max xs - foldl1 min xs
checksum sheet = sum $ map cleanDivision sheet
  where cleanDivision xs = head $ [ x `div` y | i <- [0..length xs - 1],
                                                j <- [0..length xs - 1],
                                                i /= j,
                                                let x = xs !! i,
                                                let y = xs !! j,
                                                x `mod` y == 0 ]

main :: IO ()
main = do input <- getContents
          let rows = map words $ lines input
              sheet = map (map read) rows
          print $ checksum sheet
