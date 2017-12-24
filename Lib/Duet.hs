module Duet where

import ListZipperB

import Data.Map.Strict (Map)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser

type Reg = Char
data Val = RegV Reg
         | NumV Integer
data Inst = Snd Val
          | Rcv Reg
          | Upd UpdOp Reg Val
          | Jump Test Val Val
data UpdOp = Add
           | Mod
           | Mul
           | Set
           | Sub deriving Eq
data Test = GZ
          | NZ
type Prog = [Inst]
type PC = ListZipper Inst
type Env = Map Reg Integer

regP :: Parser Reg
regP = letter

numP :: Parser Integer
numP = do sign <- option 1 (-1 <$ char '-')
          value <- read <$> some digit
          return $ sign * value

valP :: Parser Val
valP = choice [ RegV <$> regP
              , NumV <$> numP
              ]

updOpP :: Parser UpdOp
updOpP = choice [ Add <$ string "add"
                , try $ Mod <$ string "mod"
                , Mul <$ string "mul"
                , try $ Set <$ string "set"
                , Sub <$ string "sub"
                ]

jmpOpP :: Parser Test
jmpOpP = choice [ try $ GZ <$ string "jgz"
                , NZ <$ string "jnz"
                ]

instP :: Parser Inst
instP = choice [ try $ Snd <$ string "snd" <* spaces <*> valP
               , Upd <$> updOpP <* spaces <*> regP <* spaces <*> valP
               , Rcv <$ string "rcv" <* spaces <*> regP
               , Jump <$> jmpOpP <* spaces <*> valP <* spaces <*> valP
               ]

programP :: Parser Prog
programP = instP `sepEndBy1` newline

runUpdOp :: UpdOp -> Integer -> Integer -> Integer
runUpdOp Add x y = x + y
runUpdOp Mod x y = x `mod` y
runUpdOp Mul x y = x * y
runUpdOp Set _ y = y
runUpdOp Sub x y = x - y

test :: Test -> Integer -> Bool
test GZ x = x > 0
test NZ x = x /= 0
