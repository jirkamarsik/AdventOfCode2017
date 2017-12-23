module Main where

digitSum :: [Int] -> Int
digitSum series = sum $ map fst equalNeighbors
  where equalNeighbors = filter (uncurry (==)) neighbors
        neighbors = take (length series) $ pairwise $ cycle series
        -- pairwise xs = zip xs (tail xs)
        pairwise xs = zip xs (drop (length series `div` 2) xs)

parseDigits :: String -> [Int]
parseDigits = map (\c -> read [c])

main :: IO ()
main = do series <- parseDigits <$> getLine
          print $ digitSum series
