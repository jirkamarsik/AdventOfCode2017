module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GraphComponents
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

type Node = Int

nodeP :: Parser Node
nodeP = read <$> some digit

nodeDefP :: Parser (Node, [Node])
nodeDefP = do source <- nodeP
              _ <- string " <-> "
              targets <- nodeP `sepBy1` string ", "
              return (source, targets)

graphDefP :: Parser (Graph Node)
graphDefP = M.fromList <$> nodeDefP `sepEndBy1` newline

main :: IO ()
main = do input <- getContents
          case parseString (graphDefP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            -- Success g -> print $ S.size $ reachable g 0
            Success g -> print $ length $ findComponents g
