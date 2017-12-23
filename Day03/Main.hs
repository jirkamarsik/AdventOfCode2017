module Main where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

type Position = (Int, Int)

trail :: [Position]
trail = (0, 0) : concat [circleAround (-i, -i) (1 + 2 * i) | i <- [0..]]
  where circleAround (x, y) stride = [(x + i, y) | i <- [1..stride]] ++
                                     [(x + stride, y + i) | i <- [1..stride]] ++
                                     [(x + stride - i, y + stride) | i <- [1..stride+1]] ++
                                     [(x - 1, y + stride - i) | i <- [1..stride+1]]

manhattanNorm :: Position -> Int
manhattanNorm (x, y) = abs x + abs y

neighbors :: Position -> [Position]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

sums :: [Int]
sums = evalState (mapM computePosition trail) (Map.singleton (0, 0) 1)
  where computePosition :: Position -> State (Map.Map Position Int) Int
        computePosition pos = do board <- get
                                 let value = sum $ map (\npos -> Map.findWithDefault 0 npos board) (neighbors pos)
                                 put $ Map.insert pos value board
                                 return value

main :: IO ()
-- main = do index <- readLn
--           print $ manhattanNorm $ trail !! (index - 1)
main = do lowerBound <- readLn
          print $ head $ filter (> lowerBound) sums
