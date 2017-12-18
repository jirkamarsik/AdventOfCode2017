module Main where

import KnotHash

import Data.Char (ord)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

lengthListP :: Parser [Int]
lengthListP = (read <$> some digit) `sepBy1` char ','

main :: IO ()
main = do input <- getContents
          -- case parseString (lengthListP <* eof) mempty input of
            -- Failure e -> print $ _errDoc e
            -- Success lengths -> print $ hashOfChain $ chainKnot lengths
          putStrLn $ showHash $ fullKnotHash (trim input)
