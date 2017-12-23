module Main where

import Control.Applicative
import Data.Maybe (catMaybes)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

data River = Group [River] | Garbage String

groupP :: Parser River
groupP = between (char '{') (char '}') (Group <$> (riverP `sepBy` char ','))

garbageP :: Parser River
garbageP = do _ <- char '<'
              garbageElems <- manyTill garbageElemP (char '>')
              return $ Garbage $ catMaybes garbageElems
  where garbageElemP = Nothing <$ (char '!' *> anyChar) <|> Just <$> noneOf "!>"

riverP :: Parser River
riverP = groupP <|> garbageP

score :: River -> Int
score = score' 1
  where score' :: Int -> River -> Int
        score' base (Group rivulets) = base + sum (map (score' (base + 1)) rivulets)
        score' _    (Garbage _)      = 0

countGarbage :: River -> Int
countGarbage (Group rivulets) = sum $ map countGarbage rivulets
countGarbage (Garbage dump)   = length dump

main :: IO ()
main = do input <- getContents
          case parseString (riverP <* spaces <* eof) mempty input of
            Failure e -> print $ _errDoc e
            -- Success river -> print $ countGarbage river
            Success river -> print $ countGarbage river
