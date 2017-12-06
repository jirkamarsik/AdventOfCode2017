module Main where

import ListZipper
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import Data.Maybe (fromJust)

type Memory = [Int]

advance :: Memory -> Memory
advance mem = toList finalState
  where initialState = fromJust $ locate (maximum mem) mem
        toDivide = getCursor initialState
        cleanedState = updateCursor (const 0) initialState
        finalState = iterate (updateCursor (+1) . shiftRight) cleanedState !! toDivide

-- firstRepeated :: Ord a => (a -> a) -> a -> Int
-- firstRepeated f start = search series 0 Set.empty
--   where series = iterate f start
--         search (x:xs) i s = if x `Set.member` s then
--                               i
--                             else
--                               search xs (i + 1) (Set.insert x s)

loopLength :: Ord a => (a -> a) -> a -> Int
loopLength f start = search series 0 Map.empty
  where series = iterate f start
        search (x:xs) i m = case Map.lookup x m of
                              Just j -> i - j
                              Nothing -> search xs (i + 1) (Map.insert x i m)

main :: IO ()
main = do input <- getContents
          let mem = map read $ words input
          print $ loopLength advance mem
