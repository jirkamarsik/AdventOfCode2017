module KnotHash
  ( module KnotHash
  , module ListZipperB
  ) where

import Data.Bits (xor)
import Data.Char (isSpace, ord)
import ListZipperB
import Numeric (showHex)

type Chain = ListZipper Int

initChain :: Chain
initChain = LZ [] [0..255]

hashOfChain :: Chain -> Int
hashOfChain chain = a * b
  where a:b:_ = toList chain

chainKnot :: [Int] -> Chain
chainKnot = twist initChain 0
  where twist :: Chain -> Int -> [Int] -> Chain
        twist chain _        []     = chain
        twist chain skipSize (l:ls) = twist chain' (skipSize + 1) ls
          where chain' = advanceC (l + skipSize) $ flipNextC l chain

salt :: [Int]
salt = [ 17, 31, 73, 47, 23 ]

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

denseHash :: Chain -> [Int]
denseHash chain = map (foldl1 xor) $ chunkify 16 $ toList chain

showHash :: [Int] -> String
showHash = concatMap printDigit
  where printDigit x = padLeft '0' 2 $ showHex x ""
        padLeft c n cs = replicate (n - length cs) c ++ cs

fullKnotHash :: String -> [Int]
fullKnotHash str = denseHash $ chainKnot $ concat $ replicate 64 lengths
  where lengths = map ord str ++ salt

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
