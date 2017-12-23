module Main where

import Control.Monad (guard)
import Data.List (elemIndex, group, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import Text.Parsec
import Text.Parsec.String

data ProgramDef = PD { getName :: String
                     , getWeight :: Int
                     , getSubprograms :: [String]
                     }

lookupUnsafe :: Ord a => a -> Map.Map a b -> b
lookupUnsafe k m = fromJust $ Map.lookup k m

programNameP :: Parser String
programNameP = many1 lower

weightP :: Parser Int
weightP = read <$> many1 digit

lineP :: Parser ProgramDef
lineP = do name <- programNameP
           _ <- char ' '
           weight <- between (char '(') (char ')') weightP
           subprograms <- option [] $ string " -> " *> programNameP `sepBy1` string ", "
           return $ PD name weight subprograms

programP :: Parser (Map.Map String ProgramDef)
programP = Map.fromList . map (\pd -> (getName pd, pd)) <$> lineP `endBy1` newline

findParents :: Map.Map String ProgramDef -> Map.Map String String
findParents pds = Map.fromList $ [ (sub, super) | PD super _ subs <- Map.elems pds, sub <- subs ]

findRoot :: Map.Map String String -> String -> String
findRoot parents node = case Map.lookup node parents of
                          Just parent -> findRoot parents parent
                          Nothing -> node

subtowerWeight :: Map.Map String ProgramDef -> String -> Int
subtowerWeight pds towerRoot = weight + sum (map (subtowerWeight pds) subs)
  where PD _ weight subs = lookupUnsafe towerRoot pds

findFixes :: Map.Map String ProgramDef -> String -> Int -> [Int]
findFixes pds node disbalance = balanced ++ twoUnbalanced ++ lotsUnbalanced
  where balanced = do guard $ allEqual subtowerWeights
                      guard $ disbalance /= 0
                      return $ weight - disbalance
        twoUnbalanced = do guard $ length subs == 2
                           guard $ not $ allEqual subtowerWeights
                           let [subA, subB] = subs
                               [weightA, weightB] = subtowerWeights
                           recurse subA (weightA - weightB) ++ recurse subB (weightB - weightA)
        lotsUnbalanced = do guard $ length subs > 2
                            let allWeights = sortWith length $ group $ sort subtowerWeights
                            guard $ length allWeights == 2 && length (allWeights !! 0) == 1
                            let [ wrongWeight:_, correctWeight:_ ] = allWeights
                            let wrongSub = subs !! fromJust (elemIndex wrongWeight subtowerWeights)
                            recurse wrongSub (wrongWeight - correctWeight)
        recurse sub subDisbalance = do guard $ disbalance == 0 || subDisbalance == disbalance
                                       findFixes pds sub subDisbalance
        subtowerWeights = map (subtowerWeight pds) subs
        PD _ weight subs = lookupUnsafe node pds
        allEqual (x:xs) = all (== x) xs
        allEqual []     = True

main :: IO ()
main = do input <- getContents
          case parse programP "(stdin)" input of
            Left e -> print e
            Right pds -> let parents = findParents pds
                             someNode = head $ Map.keys pds
                             root = findRoot parents someNode
                             possibleFixes = findFixes pds root 0 in
                         print $ head possibleFixes
