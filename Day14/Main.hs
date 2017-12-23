module Main where

import GraphComponents
import KnotHash

import Data.Bits (testBit)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Grid = [[Bool]]
type Node = (Int, Int)

hashBits :: [Int] -> [Bool]
hashBits hash = [ testBit h i | h <- hash, i <- [7,6..0] ]

buildGrid :: String -> Grid
buildGrid keyStr = map hashBits hashes
  where hashes = [ fullKnotHash $ keyStr ++ "-" ++ show i  | i <- [0..127] ]

usedMem :: Grid -> Int
usedMem = length . filter id . concat

buildGraph :: Grid -> Graph Node
buildGraph grid = edges
  where neighbors (r,c) = concatMap tryPosition [(r-1,c), (r+1,c), (r, c-1), (r, c+1)]
        tryPosition pos = if pos `S.member` nodes then
                            [pos]
                          else
                            []
        nodes = S.fromList [ (r,c) | (r, row) <- zip [0..127] grid,
                                     (c, bit) <- zip [0..127] row,
                                     bit ]
        edges = M.fromList [ (u, neighbors u) | u <- S.toList nodes ]

main :: IO ()
main = do input <- getContents
          -- print $ usedMem $ buildGrid $ trim input
          print $ length $ findComponents $ buildGraph $ buildGrid $ trim input
