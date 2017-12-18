module Main where

import ListZipperA
import Data.Maybe (fromJust, isJust)

type Tape = ListZipper Int

updateInst :: Int -> Int
-- updateInst inst = inst + 1
updateInst inst = if inst >= 3 then inst - 1 else inst + 1

advance :: Tape -> Maybe Tape
advance t@(LZ _ h _) = move h (updateCursor updateInst t)

trace :: Tape -> [Tape]
trace tape = map fromJust $ takeWhile isJust $ iterate (>>= advance) (Just tape)

main :: IO ()
main = do input <- getContents
          let x:xs = map read $ lines input
              tape = LZ [] x xs
          print $ length $ trace tape
