module Main where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

data Scanner = Scanner { _depth :: Int
                       , _range :: Int
                       }

willCatch :: Int -> Scanner -> Bool
willCatch _     (Scanner _     1)     = True
willCatch delay (Scanner depth range) = (depth + delay) `mod` (2 * (range - 1)) == 0

severity :: Scanner -> Int
severity (Scanner depth range) = depth * range

runSeverity :: [Scanner] -> Int
runSeverity ss = sum $ map severity $ filter (willCatch 0) ss

scannerP :: Parser Scanner
scannerP = do depth <- read <$> some digit
              _ <- string ": "
              range <- read <$> some digit
              return $ Scanner depth range

scannersP :: Parser [Scanner]
scannersP = scannerP `sepEndBy1` newline

safeDelay :: [Scanner] -> Int -> Bool
safeDelay ss delay = not $ any (willCatch delay) ss

main :: IO ()
main = do input <- getContents
          case parseString (scannersP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            -- Success ss -> print $ runSeverity ss
            Success ss -> print $ head $ filter (safeDelay ss) [0..]
