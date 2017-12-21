module Main where

import Data.Char (chr, ord)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

type Dancer = Char
data DanceMove = Spin Int | Exchange Int Int | Partner Dancer Dancer
type Scene = [Dancer]

numberP :: Parser Int
numberP = read <$> some digit

danceMoveP :: Parser DanceMove
danceMoveP = choice [ Spin <$ char 's' <*> numberP
                    , Exchange <$ char 'x' <*> numberP <* char '/' <*> numberP
                    , Partner <$ char 'p' <*> anyChar <* char '/' <*> anyChar
                    ]

danceP :: Parser [DanceMove]
danceP = danceMoveP `sepBy1` char ','

move :: Scene -> DanceMove -> Scene
move scene (Spin n) = suffix ++ prefix
  where (prefix, suffix) = splitAt (length scene - n) scene
move scene (Exchange i j) = move scene (Partner d_i d_j)
  where d_i = scene !! i
        d_j = scene !! j
move scene (Partner a b) = map (\d -> if d == a then b else if d == b then a else d) scene

type Perm a = a -> a

exponentiatePerm :: Eq a => Int -> Perm a -> Perm a
exponentiatePerm n p x = elemCycle !! cycleIndex
  where elemCycle = x : (takeWhile (/= x) $ iterate p (p x))
        cycleIndex = n `mod` length elemCycle

main :: IO ()
main = do input <- getContents
          case parseString (danceP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            Success dance -> putStrLn finalScene
              where initialScene = ['a'..'p']
                    -- finalScene = foldl move initialScene dance
                    isPartner (Partner _ _) = True
                    isPartner _             = False
                    positionScene = foldl move initialScene $ filter (not . isPartner) dance
                    invPosPerm i = ord (positionScene !! i) - ord 'a'
                    positionExpScene = map (\x -> chr (ord 'a' + exponentiatePerm 1000000000 invPosPerm x)) [0..15]
                    chrScene = foldl move initialScene $ filter isPartner dance
                    chrPerm c = chrScene !! (ord c - ord 'a')
                    finalScene = map (exponentiatePerm 1000000000 chrPerm) positionExpScene
