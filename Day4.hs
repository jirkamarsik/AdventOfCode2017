module Main where

import Data.List (sort)
import qualified Data.Set as Set

-- noDuplicates :: [String] -> Bool
-- noDuplicates wordList = length wordList == Set.size wordSet
--   where wordSet = Set.fromList wordList

noAnagrams :: [String] -> Bool
noAnagrams wordList = length wordList == Set.size anagramSet
  where anagramSet = Set.fromList $ map sort wordList

main :: IO ()
main = do input <- getContents
          let passphrases = map words $ lines input
          -- print $ length $ filter noDuplicates passphrases
          print $ length $ filter noAnagrams passphrases
