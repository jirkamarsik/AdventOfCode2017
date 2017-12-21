module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Either (partitionEithers)
import Data.List (intersperse, transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result (_errDoc)
import qualified Text.Trifecta.Result as TR

import Test.QuickCheck

type Pixel = Bool
data Block2 = B2 { unB2 :: [Pixel] } deriving (Eq, Ord)
data Block3 = B3 { unB3 :: [Pixel] } deriving (Eq, Ord)
data Block4 = B4 { unB4 :: [Pixel] }
data Rule = B2B3 Block2 Block3 | B3B4 Block3 Block4
data Painting = P { unP :: [[Pixel]] } deriving Eq
data Rules = R (Map Block2 Block3) (Map Block3 Block4)

lookupUnsafe :: Ord k => k -> Map k v -> v
lookupUnsafe k m = fromJust $ Map.lookup k m

pixelP :: Parser Pixel
pixelP = True <$ char '#' <|> False <$ char '.'

blockP :: Int -> Parser [Pixel]
blockP blockSize = concat <$> sequence parserChain
  where pixelParsers = take blockSize $ repeat (replicateM blockSize pixelP)
        parserChain = intersperse ([] <$ char '/') pixelParsers

block2P :: Parser Block2
block2P = B2 <$> blockP 2

block3P :: Parser Block3
block3P = B3 <$> blockP 3

block4P :: Parser Block4
block4P = B4 <$> blockP 4

ruleP :: Parser Rule
ruleP = choice [ try $ B2B3 <$> block2P <* string " => " <*> block3P
               , B3B4 <$> block3P <* string " => " <*> block4P ]

programP :: Parser Rules
programP = do rules <- ruleP `sepEndBy1` newline
              let (rules23, rules34) = partitionEithers $ map thisOrThat rules
              return $ R (buildMap allVariants2 rules23) (buildMap allVariants3 rules34)
  where thisOrThat (B2B3 l r) = Left (l, r)
        thisOrThat (B3B4 l r) = Right (l, r)
        buildMap findVariants theRules = Map.fromList $ do (l, r) <- theRules
                                                           l' <- findVariants l
                                                           return (l', r)

initialState :: Painting
initialState = P [ [ False,  True, False ]
                 , [ False, False,  True ]
                 , [  True,  True,  True ] ]

permutate2 :: Block2 -> [Block2]
permutate2 (B2 [ a, b
               , c, d ]) = [ B2 [ b, d   -- Rotate
                                , a, c ]
                           , B2 [ c, d   -- Flip
                                , a, b ] ]

permutate3 :: Block3 -> [Block3]
permutate3 (B3 [ a, b, c
               , d, e, f
               , g, h, i ]) = [ B3 [ c, f, i   -- Rotate
                                   , b, e, h
                                   , a, d, g ]
                              , B3 [ g, h, i   -- Flip
                                   , d, e, f
                                   , a, b, c ] ]

transitiveReflexiveClosure :: Ord a => (a -> [a]) -> a -> [a]
transitiveReflexiveClosure rel x = Set.toList $ buildClosure Set.empty (Set.singleton x)
  where buildClosure old new = if new `Set.isSubsetOf` old then
                                 old
                               else
                                 buildClosure (Set.union old new) (Set.fromList $ concatMap rel $ Set.toList new)

allVariants2 :: Block2 -> [Block2]
allVariants2 = transitiveReflexiveClosure permutate2

allVariants3 :: Block3 -> [Block3]
allVariants3 = transitiveReflexiveClosure permutate3

carZip :: [[a]] -> [a]
carZip xs | length xs > 0 && length (head xs) > 0 = map head xs ++ carZip (map tail xs)
carZip xs                                         = []

layout3 :: [[Block3]] -> Painting
layout3 grid = P $ carZip $ map getRow [0..2]
  where getRow i = map (concatMap (take 3 . drop (3 * i) . unB3)) grid

layout4 :: [[Block4]] -> Painting
layout4 grid = P $ carZip $ map getRow [0..3]
  where getRow i = map (concatMap (take 4 . drop (4 * i) . unB4)) grid

carUnzip :: Int -> [a] -> [[a]]
carUnzip n xs | length xs > 0 = take n xs : carUnzip n (drop n xs)
carUnzip n []                 = []

chop2 :: Painting -> [[Block2]]
chop2 (P grid) = map (map (B2 . concat . transpose) . carUnzip 2 . transpose) $ carUnzip 2 grid

chop3 :: Painting -> [[Block3]]
chop3 (P grid) = map (map (B3 . concat . transpose) . carUnzip 3 . transpose) $ carUnzip 3 grid

size :: Painting -> Int
size (P grid) = length grid

enhance :: Rules -> Painting -> Painting
enhance (R rules _) p | size p `mod` 2 == 0 = layout3 $ map (map (\b -> lookupUnsafe b rules)) $ chop2 p
enhance (R _ rules) p = layout4 $ map (map (\b -> lookupUnsafe b rules)) $ chop3 p

countPixels :: Painting -> Int
countPixels (P grid) = length $ [ () | row <- grid, pixel <- row, pixel ]

showPixel :: Pixel -> Char
showPixel True  = '#'
showPixel False = '.'

instance Show Painting where
  show (P grid) = unlines $ map (map showPixel) grid

main :: IO ()
main = do input <- getContents
          case parseString (programP <* eof) mempty input of
            TR.Failure e -> print $ _errDoc e
            -- TR.Success rules -> let finalState = iterate (enhance rules) initialState !! 5 in
            TR.Success rules -> let finalState = iterate (enhance rules) initialState !! 18 in
                                print $ countPixels finalState

instance Arbitrary Painting where
  arbitrary = sized (\n -> P <$> (vectorOf n $ vector n))
  shrink p = map (\n -> P $ drop n $ map (drop n) $ unP p) $ filter (<= size p) [3,2,1]

prop_chopLayout :: Painting -> Property
prop_chopLayout p = size p `mod` 3 == 0 ==> layout3 (chop3 p) === p
