module Main where

-- import Data.Char (isLetter)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

data Pos = P Int Int deriving (Eq, Ord)
data Dir = D Int Int
type Tubes = Map Pos Char

(<+>) :: Pos -> Dir -> Pos
(P x y) <+> (D dx dy) = P (x + dx) (y + dy)

turnLeft :: Dir -> Dir
turnLeft (D dx dy) = D dy (-dx)

turnRight :: Dir -> Dir
turnRight (D dx dy) = D (-dy) dx

spellPath :: Tubes -> Int -> Int -> Pos -> Dir -> String
spellPath tubes width height pos dir =
  case M.lookup (pos <+> dir) tubes of
    Just c -> c : spellPath tubes width height (pos <+> dir) dir
    Nothing -> let newDirs = filter (\d -> (pos <+> d) `M.member` tubes) $ [turnLeft dir, turnRight dir] in
               case newDirs of
                 d:_ -> spellPath tubes width height pos d
                 []  -> []

main :: IO ()
main = do input <- getContents
          let charTubes = lines input
              height = length charTubes
              width = length $ head charTubes
              tubes = M.fromList [ (P x y, c) | (y, row) <- zip [0..] charTubes,
                                                (x, c)   <- zip [0..] row,
                                                c /= ' ' ]
              startPos = P (fromJust $ elemIndex '|' $ head charTubes) (-1)
              startDir = D 0 1
          -- putStrLn $ filter isLetter $ spellPath tubes width height startPos startDir
          print $ length $ spellPath tubes width height startPos startDir
