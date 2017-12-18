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

main :: IO ()
main = do input <- getContents
          case parseString (danceP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            Success dance -> putStrLn finalScene
              where initialScene = ['a'..'p']
                    -- finalScene = foldl move initialScene dance
                    numIterations = 1000000000
                    sceneSteps = iterate iterateScene initialScene
                    sceneCycle = initialScene : takeWhile (/= initialScene) (tail sceneSteps)
                    cycleIndex = numIterations `mod` length sceneCycle
                    finalScene = sceneCycle !! cycleIndex
                    iterateScene scene = foldl move scene dance
