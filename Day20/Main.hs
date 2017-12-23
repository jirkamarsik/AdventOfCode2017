module Main where

import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import GHC.Exts (groupWith)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

data Vec3 = V3 Integer Integer Integer deriving (Eq, Ord, Show)
data Particle = Part { _pos :: Vec3
                     , _vel :: Vec3
                     , _acc :: Vec3
                     } deriving (Eq, Show)

toList :: Vec3 -> [Integer]
toList (V3 x y z) = [x, y, z]

(<+>) :: Vec3 -> Vec3 -> Vec3
(V3 x1 y1 z1) <+> (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

norm :: Vec3 -> Integer
norm (V3 x y z) = sum $ map abs [x, y, z]

argmin :: Ord b => (a -> b) -> [a] -> a
argmin f (x:xs) = argmin' f xs x
  where argmin' f []     m = m
        argmin' f (c:cs) m = if f c < f m then argmin' f cs c else argmin' f cs m

numberP :: Parser Integer
numberP = do sign <- option 1 (-1 <$ char '-')
             value <- read <$> some digit
             return $ sign * value

vectorP :: Parser Vec3
vectorP = do [x, y, z] <- between (char '<') (char '>') $ numberP `sepBy1` char ','
             return $ V3 x y z

particleP :: Parser Particle
particleP = do [p, v, a] <- labelledVectorP `sepBy1` string ", "
               return $ Part p v a
  where labelledVectorP :: Parser Vec3
        labelledVectorP = letter *> char '=' *> vectorP

compareInf :: Particle -> Particle -> Ordering
compareInf part1 part2 = compare (lex part1) (lex part2)
  where lex (Part pos vel acc) = [ norm acc
                                 , sum $ zipWith velContrib (toList vel) (toList acc)
                                 , sum $ zipWith3 posContrib (toList pos) (toList vel) (toList acc) ]
        velContrib v a = if a < 0 then -v else if a > 0 then v else abs v
        posContrib p v a = if a < 0 then -p else if a > 0 then p else if v < 0 then -p else if v > 0 then p else abs p

compareHere :: Particle -> Particle -> Ordering
compareHere (Part pos1 _ _) (Part pos2 _ _) = compare (norm pos1) (norm pos2)

isStable :: [Particle] -> Bool
isStable parts = sortBy compareInf parts == sortBy compareHere parts

simulateTick :: [Particle] -> [Particle]
simulateTick parts = removeCollisions $ map advancePart parts
  where advancePart (Part pos vel acc) = Part newPos newVel acc
          where newVel = vel <+> acc
                newPos = pos <+> newVel
        removeCollisions = concat . filter (\g -> length g <= 1) . groupWith _pos

simulateToConvergence :: [Particle] -> [Particle]
simulateToConvergence parts = if isStable parts then parts else simulateToConvergence $ iterate simulateTick parts !! 10

main :: IO ()
main = do input <- getContents
          case parseString (particleP `sepEndBy1` newline <* eof) mempty input of
            Failure e -> print $ _errDoc e
            -- Success parts -> print $ fromJust $ argmin (norm . _acc) parts `elemIndex` parts
            Success parts -> print $ length $ simulateToConvergence parts
