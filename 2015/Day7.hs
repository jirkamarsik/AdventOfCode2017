module Main where

import Data.Bits
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Word
import Text.Parsec
import Text.Parsec.String

type Wires = Map.Map String Word16
type Expr = Wires -> Word16

lookupUnsafe :: Ord a => a -> Map.Map a b -> b
lookupUnsafe k m = fromJust $ Map.lookup k m

identifier :: Parser String
identifier = many1 lower

literalExpr :: Parser Expr
literalExpr = do lit <- read <$> many1 digit
                 return $ \_ -> lit

variableExpr :: Parser Expr
variableExpr = lookupUnsafe <$> identifier

atomExpr :: Parser Expr
atomExpr = literalExpr <|> variableExpr

binOp :: Parser (Word16 -> Word16 -> Word16)
binOp = (.&.) <$ string "AND" <|> (.|.) <$ string "OR"

binOpExpr :: Parser Expr
binOpExpr = do left <- atomExpr
               _ <- char ' '
               op <- binOp
               _ <- char ' '
               right <- atomExpr
               return $ \w -> left w `op` right w

notExpr :: Parser Expr
notExpr = do _ <- string "NOT "
             arg <- identifier
             return $ \w -> complement $ lookupUnsafe arg w

shiftExpr :: Parser Expr
shiftExpr = do arg <- atomExpr
               _ <- char ' '
               shiftSign <- 1 <$ string "LSHIFT" <|> -1 <$ string "RSHIFT"
               _ <- char ' '
               amount <- read <$> many1 digit
               return $ \w -> shift (arg w) (shiftSign * amount)

expr :: Parser Expr
expr = try binOpExpr <|> try notExpr <|> try shiftExpr <|> atomExpr

assignment :: Parser (String, Wires -> Word16)
assignment = do source <- expr
                _ <- string " -> "
                target <- identifier
                return (target, source)

program :: Parser [(String, Wires -> Word16)]
program = assignment `endBy1` endOfLine

main :: IO ()
main = do input <- getContents
          case parse program "(stdin)" input of
            Left e -> print e
            Right definitions -> -- let wires = Map.fromList (map (\(name, def) -> (name, def wires)) definitions) in
                                 let firstWires = Map.fromList (map (\(name, def) -> (name, def firstWires)) definitions)
                                     valueForB = lookupUnsafe "a" firstWires
                                     wires = Map.fromList ((map (\(name, def) -> (name, def wires)) definitions) ++ [("b", valueForB)]) in
                                 print $ lookupUnsafe "a" wires
